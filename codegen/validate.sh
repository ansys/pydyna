#!/usr/bin/env bash
# Codegen Validation Script
# 
# Encapsulates the complete validation workflow for the codegen system.
# Used by both developers and CI to ensure consistency.

set -euo pipefail

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default options
QUICK_MODE=false
SKIP_TESTS=false
SKIP_PRECOMMIT=false
SKIP_DEADCODE=false
COVERAGE_THRESHOLD=80
VERBOSE=false

# Parse command-line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --quick)
            QUICK_MODE=true
            shift
            ;;
        --skip-tests)
            SKIP_TESTS=true
            shift
            ;;
        --skip-precommit)
            SKIP_PRECOMMIT=true
            shift
            ;;
        --skip-deadcode)
            SKIP_DEADCODE=true
            shift
            ;;
        --coverage-threshold)
            COVERAGE_THRESHOLD="$2"
            shift 2
            ;;
        --verbose|-v)
            VERBOSE=true
            shift
            ;;
        --help|-h)
            echo "Usage: validate.sh [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --quick              Quick validation (skip tests, pre-commit, dead code)"
            echo "  --skip-tests         Skip pytest unit tests"
            echo "  --skip-precommit     Skip pre-commit checks"
            echo "  --skip-deadcode      Skip dead code detection"
            echo "  --coverage-threshold Threshold for dead code detection (default: 80)"
            echo "  --verbose, -v        Verbose output"
            echo "  --help, -h           Show this help message"
            echo ""
            echo "Examples:"
            echo "  validate.sh                    # Full validation"
            echo "  validate.sh --quick            # Fast iteration (clean + generate + diff only)"
            echo "  validate.sh --skip-tests       # Skip unit tests"
            echo "  validate.sh --coverage-threshold 90  # Stricter dead code threshold"
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            exit 1
            ;;
    esac
done

# Quick mode sets skip flags
if [ "$QUICK_MODE" = true ]; then
    SKIP_TESTS=true
    SKIP_PRECOMMIT=true
    SKIP_DEADCODE=true
fi

# Get script directory and repository root
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$REPO_ROOT"

# Track validation results
declare -a FAILED_STEPS=()
EXIT_CODE=0

# Helper functions
print_header() {
    echo ""
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}========================================${NC}"
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

print_info() {
    if [ "$VERBOSE" = true ]; then
        echo -e "${BLUE}ℹ $1${NC}"
    fi
}

# Step 1: Clean and regenerate
print_header "Step 1: Clean and Regenerate Keyword Classes"
print_info "Running: python codegen/generate.py -c"

if python codegen/generate.py -c >/dev/null 2>&1; then
    print_success "Clean completed"
else
    print_error "Clean failed"
    FAILED_STEPS+=("Clean")
    EXIT_CODE=1
fi

print_info "Running: python codegen/generate.py"
if python codegen/generate.py >/dev/null 2>&1; then
    print_success "Generation completed (3173+ classes expected)"
else
    print_error "Generation failed"
    FAILED_STEPS+=("Generation")
    EXIT_CODE=1
fi

# Step 2: Check for unintended changes
print_header "Step 2: Check for Unintended Changes (git diff)"

HAS_CHANGES=false
CHANGES_DETAILS=""

# Detect if running in CI
IN_CI=false
if [ -n "${CI:-}" ] || [ -n "${GITHUB_ACTIONS:-}" ] || [ -n "${GITLAB_CI:-}" ] || [ -n "${JENKINS_URL:-}" ]; then
    IN_CI=true
fi

# Check auto-generated keyword classes
print_info "Checking: src/ansys/dyna/core/keywords/keyword_classes/auto/"
if ! git -C src/ansys/dyna/core/keywords/keyword_classes/auto diff --relative --exit-code >/dev/null 2>&1; then
    HAS_CHANGES=true
    print_warning "Auto-generated keyword classes have changes"
    
    # Show diff stat
    if [ "$VERBOSE" = true ] || [ "$IN_CI" = true ]; then
        echo ""
        git -C src/ansys/dyna/core/keywords/keyword_classes/auto diff --relative --stat
    fi
    
    # In CI, show actual diff (limited to avoid overwhelming output)
    if [ "$IN_CI" = true ]; then
        echo ""
        echo "Git diff output (keyword classes):"
        git -C src/ansys/dyna/core/keywords/keyword_classes/auto diff --relative | head -n 200
        echo ""
    fi
fi

# Check auto-generated docs
print_info "Checking: doc/source/_autosummary/"
if ! git -C doc/source/_autosummary/ diff --relative --exit-code >/dev/null 2>&1; then
    HAS_CHANGES=true
    print_warning "Auto-generated documentation has changes"
    
    # Show diff stat
    if [ "$VERBOSE" = true ] || [ "$IN_CI" = true ]; then
        echo ""
        git -C doc/source/_autosummary/ diff --relative --stat
    fi
    
    # In CI, show actual diff (limited to avoid overwhelming output)
    if [ "$IN_CI" = true ]; then
        echo ""
        echo "Git diff output (documentation):"
        git -C doc/source/_autosummary/ diff --relative | head -n 200
        echo ""
    fi
fi

if [ "$HAS_CHANGES" = true ]; then
    print_error "Git diff detected changes - codegen output has changed"
    print_info "This means either:"
    print_info "  1. The codegen logic has a bug that needs fixing, OR"
    print_info "  2. The changes are intentional and should be committed"
    
    if [ "$IN_CI" = false ]; then
        print_info ""
        print_info "To see the full diff locally, run:"
        print_info "  git diff src/ansys/dyna/core/keywords/keyword_classes/auto/"
        print_info "  git diff doc/source/_autosummary/"
    fi
    
    FAILED_STEPS+=("Git diff check")
    EXIT_CODE=1
else
    print_success "No changes detected - codegen output is stable"
fi

# Step 3: Run pre-commit hooks
if [ "$SKIP_PRECOMMIT" = false ]; then
    print_header "Step 3: Run Pre-commit Hooks"
    print_info "Running: pre-commit run --all-files"
    
    if pre-commit run --all-files; then
        print_success "Pre-commit checks passed"
    else
        print_error "Pre-commit checks failed"
        FAILED_STEPS+=("Pre-commit")
        EXIT_CODE=1
    fi
else
    print_header "Step 3: Run Pre-commit Hooks"
    print_warning "Skipped (use --skip-precommit=false to enable)"
fi

# Step 4: Dead code detection
if [ "$SKIP_DEADCODE" = false ]; then
    print_header "Step 4: Dead Code Detection"
    print_info "Running: python codegen/find_dead_code.py --threshold $COVERAGE_THRESHOLD"
    
    if python codegen/find_dead_code.py --threshold "$COVERAGE_THRESHOLD"; then
        print_success "Dead code detection passed (threshold: ${COVERAGE_THRESHOLD}%)"
    else
        print_warning "Dead code detection found issues (threshold: ${COVERAGE_THRESHOLD}%)"
        print_info "Review coverage report: codegen/coverage_html/index.html"
        # Don't fail the build for dead code warnings
    fi
else
    print_header "Step 4: Dead Code Detection"
    print_warning "Skipped (use --skip-deadcode=false to enable)"
fi

# Step 5: Unit tests
if [ "$SKIP_TESTS" = false ]; then
    print_header "Step 5: Unit Tests"
    print_info "Running: pytest -m codegen"
    
    if pytest -m codegen; then
        print_success "Unit tests passed"
    else
        print_error "Unit tests failed"
        FAILED_STEPS+=("Unit tests")
        EXIT_CODE=1
    fi
else
    print_header "Step 5: Unit Tests"
    print_warning "Skipped (use --skip-tests=false to enable)"
fi

# Summary
print_header "Validation Summary"

if [ ${#FAILED_STEPS[@]} -eq 0 ]; then
    print_success "All validation steps passed!"
    echo ""
else
    print_error "Validation failed in ${#FAILED_STEPS[@]} step(s):"
    for step in "${FAILED_STEPS[@]}"; do
        echo -e "  ${RED}✗ $step${NC}"
    done
    echo ""
fi

exit $EXIT_CODE

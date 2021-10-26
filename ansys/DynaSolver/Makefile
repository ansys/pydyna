GRPC_PYTHON_PLUGIN = grpc_python_plugin
GRPC_PYTHON_PLUGIN_PATH ?= `which $(GRPC_PYTHON_PLUGIN)`

# PROTOS_PATH = ../../protos
PROTOS_PATH = .

vpath %.proto $(PROTOS_PATH)

# Rule for producing the Python protobuf bindings
dynasolver_pb2.py: dynasolver.proto
	protoc --python_out=. --grpc_out=. --plugin=protoc-gen-grpc=$(GRPC_PYTHON_PLUGIN_PATH) $<

"""Test Ollama installation and Mistral model."""
import ollama

promt ="Hello, Ollama! Can you respond to this message?"

response_fields = ollama.chat(
        model='deepseek-r1',
        messages=[{'role': 'user', 'content': promt}],
    )

print(response_fields['message']['content'])
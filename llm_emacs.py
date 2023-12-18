import sys
import os
import json
import llm

def get_api_key(api):
    key_map = {
        'openai': 'OPENAI_API_KEY',
        'google': 'GOOGLE_API_KEY',
        # Add more mappings as needed
    }
    return os.getenv(key_map.get(api.lower()))

def run_llm_deprecated(api, model_name, prompt, options_json=None, system_prompt=None):
    # Initialize the LLM model
    model = llm.get_model(model_name)
    
    model.key = get_api_key(api)

    # Ensure the API key is set
    if not model.key:
        return "API key not set. Please set the LLM_API_KEY environment variable."

    # handle options 
    if(api.lower() == 'google'):
        options = options_json if options_json else '{}'
    else:
        options = json.loads(options_json) if options_json else {}

    # Run the prompt
    response = model.prompt(prompt, options, system_prompt)

    for chunk in response:
        print(chunk, end="")
    

def run_llm(model_name, prompt):
    model = llm.get_model(model_name)    
    response = model.prompt(prompt)
    print("<<LLM_START>>", end="")
    for chunk in response:
        print(chunk, end="")
    print("<<LLM_END>>")

def test_output():
    print("<<LLM_START>>", end="")
    print("oogie boogie", end="")
    print("<<LLM_END>>")

# class LLMWrapper:
#     model = None

#     def get_model():
#         return model
    
#     def set_model(model_name):
#         model = llm.get_model(model_name)
    
# if __name__ == "__main__":
#     api = sys.argv[1]
#     model_name = sys.argv[2]
#     prompt = sys.argv[3]
#     options = sys.argv[4] if len(sys.argv) > 4 else '{}'
#     system_prompt = sys.argv[5] if len(sys.argv) > 5 else None

#     run_llm(api, model_name, prompt, options, system_prompt)

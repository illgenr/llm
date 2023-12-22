import sys
import os
import json
import llm
import pickle

def get_api_key(api):
    key_map = {
        'openai': 'OPENAI_API_KEY',
        'google': 'GOOGLE_API_KEY',
        # Add more mappings as needed
    }
    return os.getenv(key_map.get(api.lower()))
    
def test_output():
    print("<<LLM_START>>", end="")
    print("oogie boogie", end="")
    print("<<LLM_END>>")

class LLMWrapper:
    model = None
    api = None
    conversation = None  # To hold the conversation state

    def __init__ (self):
        self.model = llm.get_model("gemini-pro")
        self.api = 'google'
    
    def set_model(self, model_name):
        self.model = llm.get_model(model_name)

    def set_api(self, api):
        self.api = api

    def prompt(self, prompt):
        response = self.model.prompt(prompt)
        print(f"**USER:** {response.prompt.prompt}")
        print(f"**AI:** ")
        for chunk in response:
            print(chunk, end="\n")
        print('\u0004') # end of transmission

    # Conversation
    def start_conversation(self):
        self.conversation = llm.Conversation(model=self.model)

    def continue_conversation(self, prompt):
        if self.conversation is None:
            self.start_conversation()
        response = self.conversation.prompt(prompt)
        print(f"**USER:** {response.prompt.prompt}")
        print(f"**AI:** ")
        for chunk in response:
            print(chunk, end="\n")
        print('\u0004')  # End of transmission

    def end_conversation(self):
        self.conversation = None

    # Save / Load
    def save_conversation(self, filepath):
        """Save the current conversation state to a file."""
        with open(filepath, 'wb') as file:
            pickle.dump(self.conversation, file)

    def load_conversation(self, filepath):
        """Load a conversation state from a file."""
        with open(filepath, 'rb') as file:
            self.conversation = pickle.load(file)

        formatted_responses = []

        model_name = self.conversation.model.model_id if self.conversation.model else 'Unknown'

        formatted_responses.append(f"MODEL:{model_name}")
        formatted_responses.append("\n")
        for response in self.conversation.responses:
            if response.prompt.prompt:
                formatted_responses.append(f"**USER:** {response.prompt.prompt}")
            if response.text():
                formatted_responses.append(f"**AI:** {response.text()}")
            formatted_responses.append("\n")            
                
        formatted_responses.append('\u0004')

        for response in formatted_responses:
            print(response)
            
    
if __name__ == "__main__":
    llmwrapper = LLMWrapper()

# An Emacs porcelain for LLM 

A set of Emacs functions that utilize the Python library of the program LLM to aid in interacting with Large Language Models.

## Installation

Step 1: 

Install LLM using `pip`:
```bash
pip install llm
```
Or using [pipx](https://pipxproject.github.io/pipx/):
```bash
pipx install llm
```
[Detailed installation instructions](https://llm.datasette.io/en/stable/setup.html).

Step 2:
Clone this repo into your .emacs.d or wherever

```bash
git clone https://github.com/illgenr/llm
```

Step 3: Configure Emacs
Add the following lines to your Emacs configuration (.emacs or init.el):
```emacs lisp
(add-to-list 'load-path "/path/to/cloned/repo")
(require 'llm)  ; Replace 'llm-emacs' with the actual file name
```

## Getting started
After installation, you can start using the package to interact with LLMs.
Make sure your api keys are exported in your .bashrc or .zshrc or use the LLM keys commands from your terminal.

```
export OPENAI_API_KEY=
export GOOGLE_API_KEY=
ect
```

Start the python adapter with
```
M-x llm-start
```

Pick an API with
```
M-x llm-set-api
```

Select a model with
```
M-x llm-select-model
```

Here are some basic commands:
```
M-x llm-run-prompt: Send a prompt to the LLM and get a response.
M-x llm-start-conversation: Initilize a conversation
M-x llm-continue-conversation: Continue a conversation
M-x llm-save-conversation: Save the current conversation.
M-x llm-load-conversation: Load a saved conversation.
```

Explore other available commands and functionalities by browsing the code.

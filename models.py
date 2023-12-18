import json
import llm

def get_models():
    models = llm.get_models_with_aliases()
    return [model_with_aliases.model.model_id for model_with_aliases in models]

if __name__ == "__main__":
    models = get_models()
    print(json.dumps(models))

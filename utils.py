def recursive_dict_update(base_dict, update_dict):
    for key, value in update_dict.items():
        if isinstance(value, dict):
            base_dict[key] = recursive_dict_update(base_dict.get(key, {}), value)
        else:
            base_dict[key] = value
    return base_dict 

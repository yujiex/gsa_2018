def idx_substr(str_list, substr):
    for i in range(len(str_list)):
        if substr in str_list[i]:
            return i
    return -1

def dist_between_adjacent(lst):
    length = len(lst)
    return [lst[i + 1] - lst[i] for i in range(length - 1)]
    
def lst_of_idx_suchthat(lst, fun):
    length = len(lst)
    result = []
    for i in range(length):
        if fun(lst[i]):
            result.append(i)
    return result
    
def merge_adjacent(lst, fun):
    length = len(lst)
    if length < 2:
        return []
    else:
        return [fun(lst[i], lst[i + 1]) for i in range(length - 1)]

import json

def identity(x):  return x

def memoise(f):
    memo = {}
    def g(*x):
        y = memo.get(x, None)
        if y is None:
            y = f(*x)
            memo[x] = y
        return y
    return g

def json_read(path):
    with open(path, 'r') as file_input:
        return json.load(file_input)

f = open("day12.txt", 'r')

sets = {}

for line in f:
    line = line.split()
    line = [line[0]] + line[2:]
    line = [s.strip(',') for s in line]

    new_set = set(line)
    for pipe in line:
        pipe = sets.get(pipe, set())
        new_set = new_set.union(pipe)
    for v in new_set:
        sets[v] = new_set

seen_sets = set([frozenset(v) for v in sets.values()])

print(len(sets['0']))
print(len(seen_sets))

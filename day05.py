f = open('input.txt', 'r')

lines = [int(x) for x in f]
print(lines)

pos = 0
pos_max = len(lines)
num_jumps = 0
while(pos < pos_max and pos >= 0):
    jump = lines[pos]
    if (jump >= 3):
        lines[pos] -= 1
    else:
        lines[pos] += 1
    pos += jump
    num_jumps += 1

print(num_jumps)

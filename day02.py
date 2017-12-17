f = open("input.txt", "r")
out = 0

for line in f:
   row = [int(x.strip()) for x in line.split("\t")]
   out += max(row) - min(row)

print("part1: ", out)
f.close()

# Part 2
f = open("input.txt", "r")
out = 0

for line in f:
   row = sorted([int(x.strip()) for x in line.split("\t")])
   found = False
   for i in range(len(row)):
       for j in range(i + 1, len(row)):
           if row[j] % row[i] is 0:
               out += int(row[j] / row[i])
               found = True
               break
       if found:
            found = False
            break

print("part2: ", out)


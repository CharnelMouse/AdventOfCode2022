packs <- readLines("03.txt")
sizes <- nchar(packs)
left <- strsplit(substr(packs, 1, sizes/2), "")
right <- strsplit(substr(packs, sizes/2 + 1, sizes), "")
shared <- mapply(intersect, left, right)
priorities <- match(shared, c(letters, LETTERS))
sum(priorities) # part one: 7811
groups <- floor((seq_along(packs) - 1)/3)
group_shared <- tapply(strsplit(packs, ""), groups, Reduce, f = intersect)
sum(match(group_shared, c(letters, LETTERS))) # part two: 2639

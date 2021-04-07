def paing(x):

    acc = 0

    for i in range(0, x+1):
        if i % 3 == 0 or i % 5 == 0:
            acc += i
    return acc

print(paing(999))

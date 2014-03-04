a = [
	[1, 2, 1],
	[7, 1, 2],
	[1, 3, 1]
]

d = 1

n, m = len(a), len(a[0])

def process_col(i, j, d, t, a):
	return map(lambda l: 0 if l < i + 1 else a[j][l] * a[i][i] - t * a[i][l], range(m))

def process_row(i, d, a):
	d *= a[i][i] ** (m - i - 1)
	a = map(lambda j: a[j] if j < i + 1 else process_col(i, j, d, a[j][i], a), range(n))
	return (d, a)

(d, a) = reduce (lambda acc, i: process_row(i, acc[0], acc[1]), range(min(n, m)), (1, a))

ans = reduce(lambda acc, i: acc * a[i][i], range(min(n, m)), 1)

if d:
	ans /= d
print '\n'.join(map(str, a))

print 'd: ', d
print 'ans: ', ans

import pseudo2, pseudo

echo parse("""
%n->O[n^2]
Int->Int
def examplezero(n):
  var result = 0
  for i in 0 ..< n:
    for j in 0 ..< n:
      result = result + j
  return result
"""
)

# for i in 0 ..< n
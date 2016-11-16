# Module mutableseqs

import sequtils,algorithm,tables
type 
  WtPair*[T] = tuple[
    item1: T,
    item2: T,
    weight: float
  ]
 
proc groupBy*[T,U](
  input: var seq[T],
  action: proc(x: T): U
): seq[tuple[a: U, b: seq[T]]] =
  var
    result = newSeq[tuple[a: U, b: seq[T]]]()
    ind: int = 0
    idxTable: Table[U, int] = initTable[U, int]()
    lx = input.high
  for idx in countdown(lx, 0):
    var item = input[idx]
    if (idxTable.hasKey(action(item))):
       result[idxTable[action(item)]].b.add(item)
    else:
      var 
        i = action(item)
        emptyS = newSeq[T]()
      emptyS.add(item)
      var  newTuple: tuple[a: U, b: seq[T]] = (i, emptyS)
      result.add(newTuple)
      idxTable.add(action(item), ind)
      ind = ind + 1
    input.delete(idx, idx)  
  return result

proc groupByReducing*[T, U, V](
  input: var seq[T],
  action: proc( x: T): U,
  transf: proc(x: T): V
): seq[tuple[a: U, b: seq[V]]] =
  var 
    result = newSeq[tuple[a: U, b: seq[V]]]()
    ind: int = 0
    idxTable = initTable[U, int]()
    lx = input.high
  for i in countdown(lx, 0):
    var item = input[i]
    if (idxTable.hasKey(action(item))):
       result[idxTable[action(item)]].b.add(transf(item))
    else:
      var 
        i = action(item)
        emptyS = newSeq[V]()
      emptyS.add(transf(item))
      var  newTuple: tuple[a: U, b: seq[V]] = (i, emptyS)
      result.add(newTuple)
      idxTable.add(action(item), ind)
      ind = ind + 1
    input.delete(i, i)
  return result

proc flatMap*[T, U]( x: var seq[T], tr: proc(y: T): seq[U]): seq[U] =
  var 
    result: seq[U] = newSeq[U]()
    lx = x.high
  for i in countdown(lx, 0):
    let
      item = x[i]
      tmp: seq[U] = tr(item)
    for z in tmp:
      result.add(z)
    x.delete(i,i)
  return result

proc flatten*[T, U](x: seq[tuple[a: U, b: seq[T]]]): seq[T] =
  var result: seq[T] = newSeq[T]()
  for item in x:
    for subitem in item.b:
      result.add(subitem)
  return result

proc transform*[T, U](x: var seq[T], act: proc(y: T): U): seq[U] =
  var 
    result = newSeq[U]()
    l = x.len - 1
  for i in countup(0, l):
    var 
      lastIdx = x.len - 1
      item  = x[lastIdx]
    result.add(act(item))
    x.delete(lastIdx, lastIdx)
  return result

proc makePairs*[T, U](
  x: seq[T], tr: proc(zz: T): U, wt: proc (zz: T): float
): seq[WtPair[U]] = 
  result = newSeq[WtPair[U]]()
  var lx = x.high
  for i in countdown(lx, 0):
    var y = x[i]
    for j in countup(0, i-1):
      var 
        z = x[j]
        t1,t2: WtPair[U]
        first: U = tr(y)
        second: U = tr(z)
        weight: float = wt(y)

      if first != second:
         t1 = (first,second,weight)
         t2 = (second,first,weight)
         result.add(t1)
         result.add(t2)
  return result

proc take*[T](x: var seq[T], numIt: int): seq[T] =
  result = newSeq[T]()
  for i in countup(0, numIt - 1):
    result.add(x[i])
  x = newSeq[T]()
  return result


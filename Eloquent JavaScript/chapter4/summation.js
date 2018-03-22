function sum(array) {
  let out = 0
  for(const n of array) {
    out += n
  }
  return out
}

function range(start, end, interval = start <= end ? 1 : -1) {
  const forwards = start < end
  const out = []
  for(i=start; forwards ? i <= end : i >= end; i += interval) {
    out.push(i)
  }
  return out
}

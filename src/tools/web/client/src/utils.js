export const toElektraBool = (val) =>
  val ? '1' : '0'

export const fromElektraBool = (val) =>
  (val === '1') ? true : false

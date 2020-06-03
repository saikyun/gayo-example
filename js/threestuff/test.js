var parseASCII = require('parse-bmfont-ascii')
var noop = function() {}

export function parseFont(data, cb) {
  var result, binary

  data = data.toString().trim()

  try {
    result = parseASCII(data)
  } catch (e) {
    console.error(e)
    cb(e)
    cb = noop
  }

  cb(null, result)
}

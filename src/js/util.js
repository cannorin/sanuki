
// @ts-check

import lzString from "lz-string";

function parseQuery() {
    var query = window.location.hash.replace(/^\#\?/, '');

    if (!query) {
      return null;
    }

    return query.split('&').map(function(param) {
      var splitPoint = param.indexOf('=');

      return {
        key : param.substring(0, splitPoint),
        value : param.substring(splitPoint + 1)
      };
    }).reduce(function(params, param){
      if (param.key && param.value) {
        params[param.key] =
          param.key === "code" || param.key === "html" || param.key === "css"
            ? lzString.decompressFromEncodedURIComponent(param.value)
            : decodeURIComponent(param.value);
      }
      return params;
    }, {});
}

export function updateQuery(code) {
    var object =
        { code : lzString.compressToEncodedURIComponent(code) };
    var query = Object.keys(object).map(function(key) {
      return key + '=' + object[key];
    }).join('&');

    window.location.hash = '?' + query;
}

export function loadState(key) {
    return Object.assign({
        code: null,
      },
      parseQuery()
    );
}

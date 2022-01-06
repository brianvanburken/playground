const cheerio = require('cheerio');
const request = require('request');
const fs = require('fs');
const crypto = require('crypto');

const baseUrl = 'https://developer.mozilla.org';
const url = `${baseUrl}/en-US/docs/Web/CSS/Reference`;
const filename = 'properties.md';

const indexQuery = '.index a:not(.new)';


const log = (val) => {
  console.log(val);
  return val;
}

const req = url => {
  return new Promise((resolve, reject) => {
    request(url, (error, response, html) => {
      if (!error && response.statusCode === 200) {
        resolve(html);
      } else {
        reject(error);
      }
    });
  });
};

req(url)
  .then(html => {
    const $ = cheerio.load(html);
    const indexEntries = $(indexQuery);
    console.log(indexEntries.length);

    return indexEntries
      .toArray()
      .map((link) => {
        const $link = $(link);
        return { name: $link.text(), url: baseUrl + link.attribs.href };
      });
  })
  // .then(urls => urls.slice(101, 150))
  // .then(urls => {
  //   return Promise.all(urls.map(({ name, url }) => {
  //     return req(url)
  //       .then(html => {
  //         const $ = cheerio.load(html);
  //         const query = '.syntaxbox';
  //         const syntax = $(query).text();
  //         let isParameter = $('#Values').length === 0;
  //         const dataRegex = RegExp('^\<(.*?)\>$')
  //         const isDataType = dataRegex.test(name);
  //         isParameter = isDataType ? false : isParameter;
  //         return { name, url, syntax, isParameter, isDataType };
  //       });
  //   }));
  // })
  // .then((data) => {
  //   return data.filter(({ isParameter, isDataType }) => {
  //     return !isParameter && !isDataType;
  //   });
  // })
  .then((data) => {
    return data
      .sort((a, b) => (a.name > b.name) ? 1 : -1 )
      .reduce((acc, { name, url, syntax }) => {
        acc += `- [ ] [\`${name}\`](${url})\n`;
        // acc += `\`\`\`${syntax}\`\`\`\n\n`;
        return acc
      }, '');
  })
  .then(markdown => {
    return new Promise((resolve, reject) => {
      fs.writeFile(filename, markdown, err => {
        if (err) {
          reject(err);
        } else {
          resolve(true);
        }
      })
    })
  })

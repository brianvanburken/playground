const cheerio = require('cheerio');
const request = require('request');
const fs = require('fs');
const csv = require('fast-csv');

(async () => {
    const cache = await new Promise((resolve) => {
        const cache = {};
        fs.createReadStream('quotes.csv')
            .pipe(csv.parse({ headers : ['quote', 'author', 'url'] }))
            .on('data', (data) => (cache[data.url] = data))
            .on('end', () => {
                resolve(cache);
            });
    });

    const url = 'https://minimalmaxims.com/quotes/page/';
    let toFetch = [];
    let isEnd = false;
    let i = -1;
    while (!isEnd) {
        const html = await req(url + (i++) + '/').catch(() => null);
        if (html !== null) {
            const $ = cheerio.load(html);
            const links = $('a.entry-title-link')
                .map((i, x) => $(x).attr('href'))
                .toArray()
                .filter((l) => !cache.hasOwnProperty(l));

            toFetch = toFetch.concat(links);
            console.log(`Links for page ${i}`, links);
            await sleep(2 * 1000);
        } else {
            isEnd = true;
        }
    }

    const quoteQuery = '.quotable-quote';
    const authorQuery = '.quoteable-author';
    const linkQuery = '.permalink';

    for (const uri of toFetch) {
        const html = await req(uri);
        const $ = cheerio.load(html);
        const quote = $(quoteQuery).text().trim();
        const author = $(authorQuery).text().replace('—', '').trim();
        const url = $(linkQuery).attr('href');
        if (!cache.hasOwnProperty(url)) {
            console.log(quote, author, url);
        }
        cache[url] = { quote, author, url };
        await sleep(2 * 1000);
    }

    const values = Object.values(cache);
    csv.writeToPath('quotes.csv', values);
})
().catch(e => {
    console.error('Error', e);
});

function sleep(ms) {
    return new Promise((resolve) => {
        setTimeout(resolve, ms);
    });
}

function req(url) {
    return new Promise((resolve, reject) => {
        request(url, (error, response, html) => {
            if (!error && response.statusCode === 200) {
                resolve(html);
            } else {
                reject(error);
            }
        });
    });
}


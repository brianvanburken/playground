const fs = require('fs');
const csv = require('fast-csv');

(async () => {
    const cache = await new Promise((resolve) => {
        const cache = [];
        fs.createReadStream('quotes.csv')
            .pipe(csv.parse({ headers : ['quote', 'author', 'url'] }))
            .on('data', (data) => (cache.push(data)))
            .on('end', () => {
                resolve(cache);
            });
    });

    const focusQuotes = cache.reduce((acc, { quote, author }) => {
        acc.push(fixQuote(quote) + ' -' + fixAuthor(author));
        return acc;
    }, []).join('\n');

    fs.writeFileSync('focus-quotes.txt', focusQuotes)
})
().catch(e => {
    console.error('Error', e);
});

function fixAuthor(author) {
    return author.replace('’', '\'');
}

function fixQuote(quote) {
    return quote
        .replace(/(?:\r\n|\r|\n)/gm, ' ')
        .replace(/\s+/gm, ' ')
        .trim();
}


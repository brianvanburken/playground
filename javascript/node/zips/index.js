console.time("total");

const fs = require('fs');
const archiver = require('archiver');

console.time("archiving");

// create a file to stream archive data to.
const output = fs.createWriteStream(__dirname + '/output.zip');
const archive = archiver('zip', {
  zlib: { level: 9 } // Sets the compression level.
});

output.on('close', function() {
  console.log(archive.pointer() + ' total bytes');
  console.log('archiver has been finalized and the output file descriptor has closed.');
  console.timeEnd("archiving");
  console.timeEnd("total");
});

archive.on('error', function(err) {
  throw err;
});

// pipe archive data to the file
archive.pipe(output);
archive.append('Hello world!', { name: 'hello_world.txt' });
archive.finalize();

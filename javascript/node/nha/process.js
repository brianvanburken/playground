import path from 'path';
import {  mkdirSync, readdirSync, readFileSync, existsSync, createWriteStream, copyFileSync, writeFileSync } from 'fs';
import jsdom from "jsdom";
import slugify from 'slugify';
import fetch from 'node-fetch';
import Epub from "epub-gen";
import { fileURLToPath } from 'url';
import crypto from 'crypto';
import Jimp from 'jimp';
import { optimize } from 'svgo';

const randomIntFromInterval = (min, max) => Math.floor(Math.random() * (max - min + 1) + min);
const delay = ms => new Promise(resolve => setTimeout(resolve, ms));
const prepareFormula = formula => `${encodeURIComponent('\\' + formula.replace(`$\\math`, "math").replace(/\$$/gmi, ''))}`;

const { JSDOM } = jsdom;

const __dirname = path.dirname(fileURLToPath(import.meta.url));

const downloadPath = path.join(__dirname, './downloads/');
const staticPath = path.join(__dirname, './assets/');
const pagesPath = path.join(__dirname, './tmp_pages/');

const cdnPath = 'https://online.nha.eu';

try { mkdirSync(staticPath, {recursive: true }); } catch (e) {}
try { mkdirSync(downloadPath, {recursive: true }); } catch (e) {}
try { mkdirSync(pagesPath, {recursive: true }); } catch (e) {}

const files = readdirSync('./');
const htmlFiles = files.filter(name => name.endsWith('.html'));

let content = [];

for (const file of htmlFiles) {
  const fileContents = readFileSync(file, { encoding: 'utf8', flag: 'r' });
  const { window } = new JSDOM(fileContents);

  // Remove some of the extra sections such as questions or homework
  const $els = Array.from(window.document.querySelectorAll('.content-budgethide'));
  for (const $el of $els) {
    $el.parentNode.removeChild($el);
  }

  const title = window.document.querySelector('h1').textContent;
  const slug = slugify(title, {
    lower: true,
    strict: true,
  });
  const htmlArticle = window.document.getElementById("rscontent").innerHTML;

  const math_blocks = Array.from(window.document.getElementsByClassName('content-math'))
    .map(el => el.textContent)
    .map(formula => ({
      formula,
      uri: prepareFormula(formula),
      hash: crypto.createHash('md5').update(formula).digest('hex'),
    }));

  let article = htmlArticle
    .replace(/<div class="content-question-question">(.*?)<\/div>/gi, '<h3>$1</h3>') // replace question in sample questions with simple h3
    .replace(/<span class="content-math.*?">(.*?)<\/span>/gi, '<math>$1</math>') // replace mathblocks
    .replace(/\sclass=".*?"\s?/gi, ' ')
    .replace(/<(.*?) >/gi, '<$1>')
    .replace(/<h2(.*?)><span.*?>(.*?)<\/span>(.*?)<\/h2>/gi, '<h2>$2 $3</h2>')
    .replace(/<h3(.*?)><span.*?>(.*?)<\/span>(.*?)<\/h3>/gi, '<h3>$2 $3</h3>')
    .replace(/<span>\s*<\/span>/gi, '')
    .replaceAll('Bekijk antwoord', '')
    .replaceAll('<a href="#hw">Ga naar huiswerk</a>', '')
    .replaceAll('<table', '<table border="1"')
    .replace(/<\w+>(\s|\n|\r)*<\/\w+>/gi, '')
    .replace('<div></div>', '')

  for (let i = 0; i < 5; i++) {
    article = article
      .replace(/<\w+>(\s|\n|\r)*<\/\w+>/gi, '')
      .replace('<div></div>', '')
      .replace(/<div>(.*?)<\/div>/gi, '$1')
  }

  // const pageToc = Array.from(window.document
  //   .querySelectorAll('#rscontent h2'))
  //   .map(el => ({
  //     text: el.textContent.replace(/([0-9\.]+)(.*?)/, '$1 $2'),
  //     id: el.getAttribute('id'),
  //   }));

  const resources = [...article.matchAll(/src="(.*?)"/gi)]
    .map(el => el[1])
    .filter(Boolean)

  for (const resource of resources) {
    try {
      const paths = resource.split('/');
      let filename = paths.pop();
      const hash = paths.pop();
      filename = `${hash}_${filename}`;
      const file = `${downloadPath}${filename}`;
      const downloadUrl = `${cdnPath}${resource}`
      const fileExists = existsSync(file);
      if (resource.startsWith('/static/image/') && !fileExists) {
        console.info(`[INFO]: downloading ${file}`);
        fetch(downloadUrl).then(res => res.body.pipe(createWriteStream(file)))
        await delay(randomIntFromInterval(4056, 17061));
      } else if (fileExists) {
        console.info(`[INFO]: already downloaded ${file}`);
      }

      const optimizedFile = `${staticPath}${filename}`;
      if (fileExists && !existsSync(optimizedFile)) {
        console.info(`[INFO]: preparing asset ${filename}`);
        const image = await Jimp.read(file);
        filename = filename.replace('.png', '.jpg');
        image
          .grayscale()
          .quality(60)
          .write(optimizedFile);
      }

      article = article.replace(resource, `file://assets/${filename}`);
    } catch(err) {
      console.error(`[ERROR]: ${err}`)
    }
  }

  for (const { formula, hash, uri } of math_blocks) {
    try {
      const filename = `${hash}.svg`;
      const file = `${downloadPath}${filename}`;
      const downloadUrl = `${cdnPath}/math?${uri}`;
      const fileExists = existsSync(file);

      if (!fileExists) {
        console.info(`[INFO]: generating math ${formula}`);
        fetch(downloadUrl).then(res => res.body.pipe(createWriteStream(file)))
        await delay(randomIntFromInterval(4056, 17061));
      } else if (fileExists) {
        console.info(`[INFO]: already generated math ${formula}`);
      }

      const optimizedFile = `${staticPath}${filename}`;
      if (fileExists && !existsSync(optimizedFile)) {
        console.info(`[INFO]: preparing asset ${filename}`);
        const svgContents = readFileSync(file);
        const optimizedSvg = optimize(svgContents, {
          path: file,
          multipass: true,
        })

        writeFileSync(optimizedFile, optimizedSvg.data);
      }

      article = article.replace(`<math>${formula}</math>`, `<img src="file://assets/${filename}">`);
    } catch(err) {
      console.error(`[ERROR]: ${err}`)
    }
  }

  writeFileSync(`${pagesPath}${slug}.html`, `<html><body>${article}`);

  content.push({
    title,
    data: article,
    filename: `${slug}`
  });
}

const timestamp = new Date().toISOString()
  .split('T')[0]
  .replace(/[-.:ZT]/gi, '');

new Epub({
  title: 'NHA Sport en Voeding',
  output: path.join(__dirname, `nha_sport_voeding_${timestamp}.epub`),
  author: 'NHA',
  lang: 'nl',
  tocTitle: 'Inhoudsopgave',
  content,
  verbose: true
});

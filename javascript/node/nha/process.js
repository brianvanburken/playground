import path from 'path';
import {  mkdirSync, readdirSync, readFileSync, existsSync, createWriteStream } from 'fs';
import jsdom from "jsdom";
import slugify from 'slugify';
import fetch from 'node-fetch';
import Epub from "epub-gen";
import { fileURLToPath } from 'url';
import Jimp from 'jimp';

const randomIntFromInterval = (min, max) => Math.floor(Math.random() * (max - min + 1) + min);
const delay = ms => new Promise(resolve => setTimeout(resolve, ms));

const { JSDOM } = jsdom;

const __dirname = path.dirname(fileURLToPath(import.meta.url));

const staticPath = path.join(__dirname, './assets/');

const cdnPath = 'https://online.nha.eu';

try { mkdirSync(staticPath, {recursive: true }); } catch (e) {}

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
  let article = htmlArticle
    .replace(/<div class="content-question-question">(.*?)<\/div>/gi, '<h3>$1</h3>') // replace question in sample questions with simple h3
    .replace(/\sclass=".*?"\s?/gi, ' ')
    .replace(/<(.*?) >/gi, '<$1>')
    .replace(/<h2(.*?)><span.*?>(.*?)<\/span>(.*?)<\/h2>/gi, '<h2$1>$2 $3</h2>')
    .replace(/<span>\s*<\/span>/gi, '')
    .replace(/<button>Bekijk antwoord<\/button>/gi, '')
    .replaceAll('<a href="#hw">Ga naar huiswerk</a>', '')
    .replaceAll('<table', '<table border="1"')

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
      const file = `${staticPath}${filename}`;
      const downloadUrl = `${cdnPath}${resource}`
      const fileExists = existsSync(file);
      if (resource.startsWith('/static/image/') && !fileExists) {
        console.info(`[INFO]: downloading ${file}`);
        fetch(downloadUrl).then(res => res.body.pipe(createWriteStream(file)))
        await delay(randomIntFromInterval(4056, 17061));
      } else if (fileExists) {
        console.info(`[INFO]: already downloaded ${file}`);
        const image = await Jimp.read(file);
        image
          .grayscale()
          .quality(80)
          .write(file);
      }

      article = article.replace(resource, `file://assets/${filename}`);
    } catch(err) {
      console.error(`[ERROR]: ${err}`)
    }
  }

  content.push({
    title,
    data: article,
    filename: `${slug}`
  });
}



new Epub({
  title: 'NHA Sport en Voeding',
  output: path.join(__dirname, 'nha_sport_voeding.epub'),
  author: 'NHA',
  lang: 'nl',
  tocTitle: 'Inhoudsopgave',
  content,
  verbose: true
});
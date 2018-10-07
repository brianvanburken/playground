FROM node:8.10

COPY . .

RUN npm install --production

CMD zip lambda.zip . | base64 -w 0

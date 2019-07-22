FROM node:carbon
WORKDIR /app

COPY package*.json ./

RUN npm install

COPY src ./src

COPY server.js .
COPY webpack.config.js .

RUN npm run-script build

EXPOSE 3000

CMD ["npm", "run-script", "start"]

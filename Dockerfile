FROM node:carbon

WORKDIR /app

COPY package*.json ./

RUN npm install

COPY src /app

CMD ["npx","webpack-dev-server"]
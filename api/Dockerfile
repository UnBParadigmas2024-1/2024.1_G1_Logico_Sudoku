FROM swipl:latest

WORKDIR /usr/src/app
COPY . .
COPY ../prolog/main.pl .

CMD ["swipl", "server.pl"]

FROM swipl:latest

# Cria um diretório para o código Prolog
WORKDIR /usr/src/app

# Copia o código Prolog para o container
COPY . .

# Comando para iniciar o SWI-Prolog e carregar o arquivo Prolog
CMD ["swipl", "main.pl"]

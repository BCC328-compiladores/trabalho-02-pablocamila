### Instalação Make e dependência LaTeX para gerar o PDF do relatório

Para garantir a compilação do relatório e adquirir o PDF, é necessário configurar o ambiente com o utilitário `make` e a distribuição LaTeX completa.

Abaixo estão os comandos que instalam o compilador, o suporte ao idioma português e os pacotes de formatação de código:

```bash
# 1. Atualizar repositórios
sudo apt update

# 2. Instalar o Make
sudo apt install make

# 3. Instalar dependências do LaTeX (Core, Português e Extras)
sudo apt install texlive-latex-base texlive-lang-portuguese \
texlive-latex-extra texlive-fonts-recommended
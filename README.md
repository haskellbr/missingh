missingh (haskellbr fork)
=========================
[**yamadapc:**](https://github.com/yamadapc)

`missingh` é uma ótima biblioteca cheia de funções úteis que não estão
disponíveis no `Prelude`. No entanto, é uma biblioteca grande e nem sempre vale
a pena a incluir em projetos, por causa do aumento no tempo de compilação e
tamanho do binário que seu uso causa.

Esse projeto pode ser interessante para pessoas querendo aprender Haskell
fazendo coisas úteis. A ideia é simples: separar o pacote `missingh` em vários
pacotes menores. No entanto, decidi seguir uma implementação um pouco mais
interessante, escrever um pacote `package-splitter` que, dado um pacote:

- [x] Lê seu manifest `.cabal`
- [x] Extrai os módulos existentes nele
- [ ] Extrai todas as dependências de cada um desses módulos
- [ ] Gera pacotes baseados nessa informação (essa é a parte mais difícil)

Ajuda que fiz algo muito parecido aos primeiros 3 passos com a ajuda do
[André Barnabá (asakeron)](https://github.com/asakeron) em
[stack-run-auto](https://github.com/yamadapc/stack-run-auto) muito recentemente.

Então só o quarto passo deve dar um pouco mais de trabalho.

## Licença
Todo o código fora do pacote `missingh` está distribuído sob a licença MIT.

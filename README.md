# SoilData-ctb: Repositório para Processamento de Dados do SoilData

## Visão Geral

Este repositório contém uma coleção de scripts em R usados para processar, limpar e padronizar conjuntos de dados do Repositório SoilData. Cada estudo está contido em sua própria pasta, com um script adaptado à sua estrutura de dados e necessidades específicas.

O objetivo é criar um fluxo de trabalho padronizado, reprodutível e colaborativo para preparar dados de solo para análises futuras.

## Estrutura do Repositório

O repositório segue uma regra simples de "uma pasta por estudo":

```
.
├── ctb0004/
│   └── ctb0004.R
├── ctb0006/
│   └── ctb0006.R
├── ...
├── helper.R
├── template_script.R
└── README.md
```
- **`ctbXXXX/`**: Um diretório para cada estudo individual, nomeado com seu ID único.
- **`helper.R`**: Uma coleção de funções auxiliares compartilhadas, usadas em vários scripts.
- **`template_script.R`**: Um script modelo que os contribuidores devem usar como ponto de partida.

## Fluxo de Contribuição

Para adicionar um novo script de processamento para um novo estudo, por favor, siga estes passos:

### 1. Configure seu Ambiente com `renv`

Este projeto usa o pacote `renv` para garantir um ambiente R reprodutível. Isso garante que todos os contribuidores usem exatamente as mesmas versões dos pacotes.

Para começar, abra o R na raiz deste repositório e execute:

```R
# Instale o renv, caso ainda não o tenha
if (!require("renv")) {
  install.packages("renv")
}

# Inicialize o ambiente do projeto (lê o arquivo renv.lock)
renv::restore()
```

### 2. Política de Dependências (Instalação de Pacotes)

**O colaborador deve, sempre que possível, utilizar os pacotes já disponíveis no projeto (`renv.lock`).** A adição de novas dependências deve ser evitada para manter o projeto leve e consistente.

Se a instalação de um novo pacote for **absolutamente necessária**, o colaborador deve primeiro **justificar a necessidade ao desenvolvedor principal** @samuel-rosa. Somente após a aprovação, o novo pacote poderá ser instalado e adicionado ao `renv`:

```R
# Após a aprovação, instale o pacote
install.packages("novo_pacote")

# Atualize o arquivo de lock para incluir a nova dependência
renv::snapshot()
```

### 3. Crie uma Nova Pasta para o Estudo

Crie uma nova pasta para o estudo, seguindo uma das regras abaixo para a nomeação:

* **Se o estudo já possui um código oficial `ctbXXXX`**, utilize esse código para nomear a pasta.
    * *Exemplo: `ctb0064`*

* **Se o estudo ainda NÃO possui um código oficial**, crie um código temporário com o prefixo `tmp` seguido de quatro dígitos.
    * *Exemplo: `tmp0001`*

### 4. Copie e Renomeie o Template

Copie o arquivo `template_script.R` do diretório raiz para a sua nova pasta e renomeie-o para corresponder ao ID do estudo.

* *Exemplo: O arquivo dentro da pasta `ctb0064` deve se chamar `ctb0064.R`.*

### 5. Curadoria e Controle de Qualidade dos Dados

O script de processamento é a principal ferramenta para validar os dados. Se, durante o desenvolvimento, você encontrar um erro ou uma inconsistência nos dados brutos:

1.  **Inicie a Investigação:** Comece o processo de validação: revise o documento de origem, consulte especialistas ou o desenvolvedor principal.
2.  **DOCUMENTO A PENDÊNCIA NO SCRIPT:** O processo de validação pode levar tempo. Para garantir que o problema não seja esquecido, **documente a pendência** na seção apropriada dentro do seu script R, incluindo a data e a descrição do problema, conforme indicado no template.
3.  **Aguarde a Resolução:** A alteração na planilha original só deve ser feita após a validação completa e confirmação da correção.
4.  **Remova a Nota de Pendência:** Uma vez que a planilha de origem for corrigida, remova a nota de pendência do script.

O objetivo é que o script processe dados já validados, mas que o trabalho de desenvolvimento não pare enquanto a curadoria está em andamento.

### 6. Desenvolva seu Script

Com os dados devidamente curados (ou com as pendências documentadas), modifique o script para realizar o processamento necessário.

### 7. Execute e Teste

Execute seu script para garantir que ele funcione sem erros e produza os arquivos de saída corretos dentro da pasta do estudo.

### 8. Envie um Pull Request

Assim que seu script estiver completo e testado, faça o commit das suas alterações e abra um Pull Request para revisão.

### 9. Uso de Inteligência Artificial Generativa

O uso de ferramentas de IA generativa é **encorajado** como apoio. Lembre-se que a responsabilidade final pela qualidade e correção do script é **sua**. Valide todo código gerado e use a IA como uma ferramenta para aprender e acelerar o desenvolvimento.

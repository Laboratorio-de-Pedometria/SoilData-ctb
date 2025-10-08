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

### 1. Política de Dependências (Instalação de Pacotes)

**O colaborador deve, sempre que possível, utilizar os pacotes já disponíveis no projeto.** A adição de novas dependências deve ser evitada para manter o projeto leve e consistente.

Se a instalação de um novo pacote for **absolutamente necessária**, o colaborador deve primeiro **justificar a necessidade ao desenvolvedor principal** @samuel-rosa. Somente após a aprovação, o novo pacote poderá ser instalado:

```R
# Após a aprovação, instale o pacote
install.packages("novo_pacote")
```

### 2. Crie uma Nova Pasta para o Estudo

Crie uma nova pasta para o estudo, seguindo uma das regras abaixo para a nomeação:

* **Se o estudo já possui um código oficial `ctbXXXX`**, utilize esse código para nomear a pasta.
    * *Exemplo: `ctb0064`*

* **Se o estudo ainda NÃO possui um código oficial**, crie um código temporário com o prefixo `tmp` seguido de quatro dígitos.
    * *Exemplo: `tmp0001`*

### 3. Use um Script Existente como Exemplo

Em vez de começar do zero, procure um script de um estudo já processado que seja semelhante ao seu novo estudo. O script `ctb0004/ctb0004.R` é um bom exemplo inicial.

Copie o script selecionado para a nova pasta do estudo e renomeie-o para corresponder ao ID do seu estudo.

**Exemplo:** Copie `ctb0004/ctb0004.R` para `tmp0001/` e renomeie para `tmp0001.R`.

### 4. Curadoria e Controle de Qualidade dos Dados

O script de processamento é a principal ferramenta para validar os dados. Se, durante o desenvolvimento, você encontrar um erro ou uma inconsistência nos dados brutos:

1.  **Inicie a Investigação:** Comece o processo de validação: revise o documento de origem, consulte especialistas ou o desenvolvedor principal.
2.  **DOCUMENTO A PENDÊNCIA NO SCRIPT:** O processo de validação pode levar tempo. Para garantir que o problema não seja esquecido, **documente a pendência** na seção apropriada dentro do seu script R, incluindo a data e a descrição do problema, conforme indicado no template.
3.  **Aguarde a Resolução:** A alteração na planilha original só deve ser feita após a validação completa e confirmação da correção.
4.  **Remova a Nota de Pendência:** Uma vez que a planilha de origem for corrigida, remova a nota de pendência do script.

O objetivo é que o script processe dados já validados, mas que o trabalho de desenvolvimento não pare enquanto a curadoria está em andamento.

### 5. Desenvolva seu Script

Com os dados devidamente curados (ou com as pendências documentadas), modifique o script para realizar o processamento necessário.

### 6. Execute e Teste

Execute seu script para garantir que ele funcione sem erros e produza os arquivos de saída corretos dentro da pasta do estudo.

### 7. Envie um Pull Request

Assim que seu script estiver completo e testado, faça o commit das suas alterações e abra um Pull Request para revisão.

### 9. Uso de Inteligência Artificial Generativa

O uso de ferramentas de IA generativa é **encorajado** como apoio. Lembre-se que a responsabilidade final pela qualidade e correção do script é **sua**. Valide todo código gerado e use a IA como uma ferramenta para aprender e acelerar o desenvolvimento.

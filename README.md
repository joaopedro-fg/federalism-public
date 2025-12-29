# Federalism and Public Administration
To extract data from Base dos Dados, the following SQL queries were used:

For municipal_public_workers_rais.csv:
```
SELECT id_municipio as municipality_id, ano as year, COUNTIF(tipo_vinculo IN ('35', '50','60','65','70','75','90','95','96','97')) as temporaries, COUNT(*) as total_employees, SUM(IF(tipo_vinculo IN ('35', '50','60','65','70','75','90','95','96','97'), valor_remuneracao_media, 0)) as total_wage_temporaries, SUM(IF(tipo_vinculo NOT IN ('35', '50','60','65','70','75','90','95','96','97'), valor_remuneracao_media, 0)) as total_wage_others, COUNTIF(tipo_vinculo IN ('35', '50','60','65','70','75','90','95','96','97') AND (grau_instrucao_1985_2005 IN ('7','8','9','10','11') OR grau_instrucao_apos_2005 IN ('7','8','9','10','11'))) as temporaries_high_school, COUNTIF(tipo_vinculo IN ('35', '50','60','65','70','75','90','95','96','97') AND (grau_instrucao_1985_2005 IN ('9','10','11') OR grau_instrucao_apos_2005 IN ('9','10','11'))) as temporaries_university  FROM `basedosdados.br_me_rais.microdados_vinculos` 
WHERE natureza_juridica IN ('1031','1120','1155','1279','1309','1333') AND ano >=1999
GROUP BY year,municipality_id
```
For electoral_data.csv:
```
WITH RankedVotes AS (
  -- Step 1: Rank candidates within each election (by year and municipality)
  -- based on votes and calculate the total votes for that election.
  SELECT
    ano,
    id_municipio,
    votos,
    -- Calculate total votes for the election
    SUM(votos) OVER (PARTITION BY ano, id_municipio) AS total_votos,
    -- Rank candidates by vote count
    RANK() OVER (PARTITION BY ano, id_municipio ORDER BY votos DESC) AS classificacao
  FROM
    `basedosdados.br_tse_eleicoes.resultados_candidato_municipio`
  WHERE
    turno = 1 AND cargo = "prefeito" AND tipo_eleicao = "eleicao ordinaria"
),
ElectionResults AS (
  -- Step 2: Pivot the data to get the winner's and runner-up's vote counts
  -- in a single row for each election.
  SELECT
    ano,
    id_municipio,
    total_votos,
    MAX(CASE WHEN classificacao = 1 THEN votos END) AS votos_vencedor,
    MAX(CASE WHEN classificacao = 2 THEN votos END) AS votos_segundo_lugar
  FROM
    RankedVotes
  GROUP BY
    ano,
    id_municipio,
    total_votos
)
-- Step 3: Calculate the margin and determine if the election was competitive.
SELECT
  ano,
  id_municipio,
  CASE
    -- Check if there is a runner-up.
    WHEN votos_segundo_lugar IS NULL THEN 0
    -- Check if the margin is less than 5%. BigQuery handles the division correctly.
    WHEN (votos_vencedor - votos_segundo_lugar) / total_votos < 0.05 THEN 1
    ELSE 0
  END AS competitividade
FROM
  ElectionResults
ORDER BY
  ano DESC,
  id_municipio;
```

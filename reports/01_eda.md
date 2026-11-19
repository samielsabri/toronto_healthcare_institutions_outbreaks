# Phase 1 — Exploratory profile of the raw outbreak files

*Extract: 2026-11-19 (data as of the 2026-11-12 refresh). Source: City of Toronto Open Data, "Outbreaks in Toronto Healthcare Institutions". Reproduce with `Rscript scripts/01_download_raw.R && Rscript scripts/02_profile_raw.R`. Every number below comes from a CSV in [`reports/tables/`](tables/).*

## 1. What was downloaded

- The package has 11 **datastore** resources (`ob_report_2016` … `ob_report_2026`) plus static file copies of each (`ob_report_YYYY.csv/.xml/.json`). The static CSVs matched the datastore exports row for row, so the pipeline uses only the datastore resources. It resolves them by name and saves the CKAN CSV export byte-for-byte to `data/raw/`. `data/raw/manifest.csv` records each resource ID, MD5 and download time.
- **7,085 rows** in total, 0.9 MB. The current-year file (2026) was last refreshed 2026-11-12. Its latest onset date is 2026-11-10.
- `opendatatoronto::show_package()` currently fails on this package (a tibble size error from the CKAN metadata in package version 0.1.6). `list_package_resources()` works and is the only call the pipeline needs.

## 2. Structure and quality

| | Finding |
|---|---|
| **Columns** | Ten columns in every year: the nine you listed plus **`Active`** (Y/N). `Active = Y` matches a blank `Date Declared Over` exactly (22 rows, all in 2026). |
| **Schema drift** | The **2020 file names the agent columns `Causative Agent - 1` / `- 2`** (spaces around the hyphen). Every other year uses `Causative Agent-1` / `-2`. Order and types are otherwise identical. A schema check in Phase 3 needs to allow for this known variant. |
| **Types** | Dates are ISO `YYYY-MM-DD` in every year, and all parse. `_id` is an integer **that restarts at 1 in each file**, so it is a row number, not a stable outbreak ID (this matters for snapshots; see §5). |
| **Missingness** | No blank names, addresses, settings, types, `Agent-1` values or onset dates. Unknown agent is coded as text, not as a blank: `Unable to identify` (666 rows) and `Pending` (2 rows: one still open in 2026, **one from 2021 never updated**). `Agent-2` is blank when there is no second agent, **except in 2023, where it holds the literal string `None`** (1,018 rows). |
| **File assignment** | **Each row sits in the file for the calendar year of its onset.** No row's onset falls outside its file's year, and outbreaks that close in the following January stay in the onset-year file. Past-year files are updated after year end: no 2025 outbreak is still open. |
| **Duplicates** | **No exact duplicates within or across files**, so the year-boundary duplication you expected does not happen in the current extract. There are **25 pairs that share the same unit and onset date but list different agents** (see §4, question Q3). |
| **Dates** | No declared-over date falls before onset, and no onset falls after the extract date. One outbreak has a 0-day duration (Nisbet Lodge 7th Fl, COVID-19, 2025-12-12). Overall median duration is 13 days (p95 38). |
| **Long outbreaks** | Seaton House (Shelter, *S. pyogenes*, 578 days, 2016–17); two *C. difficile* outbreaks (~130 days); and COVID-era LTCH outbreaks of 95–126 days. These look plausible rather than wrong. See `duration_outliers.csv`. |
| **Text quality** | Names are **truncated at 60 characters up to 2021** (e.g. `Rockcliffe Care Community - 3rd(Covid19) Other Fls (Resp. Ob`). One 2020 row is named just `- Unit 3-1` (address 1001 Queen St W, i.e. CAMH). `Agent-2` is also truncated in places (`Respiratory syncytial virus/ C`). Capitalization and spelling vary freely. |

## 3. Volume: why the counts roughly double after 2020

| Season (ISO wk 35–34) | COVID-19 | Other resp., agent known | Resp., agent unknown | Enteric | Other |
|---|--:|--:|--:|--:|--:|
| 2015/16 *(partial: data start Jan 2016)* | 0 | 131 | 8 | 26 | 0 |
| 2016/17 | 0 | 305 | 12 | 62 | 0 |
| 2017/18 | 0 | 291 | 72 | 85 | 1 |
| 2018/19 | 0 | 180 | 91 | 57 | 3 |
| 2019/20 | 179 | 136 | 43 | 41 | 5 |
| 2020/21 | 411 | 8 | 3 | 11 | 0 |
| 2021/22 | 771 | 56 | 5 | 12 | 1 |
| 2022/23 | 657 | 266 | 33 | 51 | 4 |
| 2023/24 | 731 | 310 | 42 | 55 | 4 |
| 2024/25 | 378 | 513 | 36 | 115 | 4 |
| 2025/26 | 208 | 447 | 57 | 38 | 7 |
| 2026/27 *(to wk 46)* | 32 | 72 | 16 | 2 | 1 |

At least three things drive the increase, and only one of them is disease activity:

1. **COVID-19 itself**: about 200–770 outbreaks a season on top of the usual respiratory burden.
2. **Finer-grained declaration.** The share of rows naming a unit (`" - "` in the name) rose from **60–69% (2016–2020) to 90–94% (2022 onward)**. A crude facility-level grouping (rough address match, same outbreak type, onsets within 14 days) gives **1.09–1.16 unit rows per facility episode before 2021, against 1.34–1.47 from 2022**. Part of the jump is the same event being counted on more wards. See `unit_vs_facility_rough.csv`; this is only for sizing the effect, not an analysis.
3. **More testing, more identified agents.** In 2018 and 2019, **26–29% of respiratory outbreaks had no agent identified, compared with 1–4% since 2020** (2026 to date: 10%, spread across the year). Enteric outbreaks have stayed 32–67% unknown throughout. Respiratory outbreaks with a known non-COVID agent are also higher in 2024/25–2025/26 (about 450–510) than in any pre-pandemic season (180–305).

This supports reporting pre-2020 and post-2022 baselines separately, and it also means **agent-specific pre-2020 baselines (e.g. for flu and RSV onset) are biased downward**, because many pre-2020 outbreaks have an unknown agent.

## 4. Label drift

### Outbreak setting

| Setting | 2016 | 2017 | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 | 2024 | 2025 | 2026 | Total |
|:--|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|
| Hospital-Acute Care | 28 | 38 | 48 | 39 | 82 | 102 | 248 | 107 | 89 | 89 | 35 | 905 |
| Hospital-Chronic Care | 37 | 31 | 45 | 30 | 49 | 47 | 177 | 131 | 125 | 83 | 52 | 807 |
| Hospital-Psychiatric | 2 | 6 | 8 | 5 | 9 | 3 | 17 | 19 | 25 | 12 | 8 | 114 |
| LTCH | 160 | 225 | 264 | 203 | 316 | 187 | 477 | 601 | 631 | 629 | 359 | 4052 |
| Retirement Home | 40 | 71 | 72 | 40 | 97 | 55 | 205 | 195 | 161 | 164 | 78 | 1178 |
| **Shelter** | 1 | 0 | 2 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 3 |
| **Transitional Care** | 0 | 0 | 0 | 0 | 0 | 0 | 9 | 13 | 2 | 2 | 0 | 26 |

The setting labels are stable apart from two you didn't anticipate. **Shelter** is three Seaton House outbreaks from 2016–2018. **Transitional Care** appears only from 2022 to 2025 and covers hospital-run transitional or reactivation units, often physically inside an LTCH or retirement building (e.g. Cedarbrook Lodge TCU, Reactivation Care Centre at 200 Church St, UHN Transitional Care at Queens Estate). Setting is recorded per outbreak, so a single physical site can carry different settings over time; Village of Humber Heights, for example, appears as both LTCH and Retirement Home.

### Type of outbreak

| Type | 2016 | 2017 | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 | 2024 | 2025 | 2026 |
|:--|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|
| Respiratory | 227 | 304 | 357 | 257 | 519 | 385 | 1107 | 1016 | 956 | 878 | 494 |
| Enteric | 41 | 67 | 79 | 55 | 33 | 9 | 23 | 47 | 71 | 97 | 33 |
| Other | 0 | 0 | 3 | 5 | 1 | 0 | 3 | 3 | 6 | 4 | 5 |

"Other" (30 rows) is almost all **CPE** (carbapenemase-producing Enterobacterales), plus two *S. pyogenes*, one neonatal GBS and one with agent unknown. (*S. pyogenes* also appears under Respiratory, and *Legionella* is coded Respiratory.) The type-agent pairings are internally consistent: no enteric pathogen appears under Respiratory, and no respiratory virus under Enteric.

### Causative agents: main drift points

- **COVID-19** is always the exact string `COVID-19`, always in `Agent-1`, from the first onset on **2020-03-17** onward. It **never appears in `Agent-2`**: whenever COVID is co-detected it is listed first. That suggests COVID was prioritised in Agent-1, which matters if we classify by Agent-1 alone.
- **Seasonal coronavirus** is `Coronavirus` in the 2016–2019 files and `Coronavirus*` from the 2020 file onward. The asterisk was applied retroactively to that whole file, including the January–February 2020 rows. **Nine outbreaks with `Coronavirus*` as Agent-1 have onsets in March–April 2020**, overlapping the first COVID-19 outbreaks, and one row lists both (`COVID-19` + `Coronavirus*`, Rockcliffe, 2020-03-23). I'd take them as labelled (seasonal), but they are the least certain rows in the dataset.
- **Rhinovirus/enterovirus**: `Rhinovirus`, `Enterovirus`, `Enterovirus/Rhinovirus` (to 2023), then `Enterovirus/ Rhinovirus` (with a space, 2024 onward).
- **Norovirus**: `Norovirus-like` until 2022, then `Norovirus` from 2023, with no overlap. This is probably a change in coding convention, possibly also in what counts as confirmed, rather than a change in the disease.
- **Influenza A**: subtype labels change from year to year (`(H1N1)pdm09`, `H1`, `H1N1`, `H3`, `H3N2`, `Not subtyped`). `Not subtyped` dominates from 2023.
- **Parainfluenza**: nine single-agent spellings (`type 1–4`, `Type 1`, `PIV III`, `UNSPECIFIED`, `virus`, bare).
- **CPE**: 21 free-text spellings in `Agent-1` (27 across both fields).
- **Co-detections are not confined to `Agent-2`.** `Agent-1` sometimes holds two agents (`Influenza A (H3), Parainfluenza type 1`; `Rhinovirus, Coronavirus`; `Influenza A and B`). `Agent-2` can list three or more, separated by `,`, `/` or `&` (e.g. `Parainfluenza type 2 & type 4`). Note that `/` is also part of a single label (`Enterovirus/Rhinovirus`).
- **Same unit, same onset, different agent.** 25 pairs (`same_unit_same_onset.csv`). Six are respiratory + enteric, which are clearly separate outbreaks. Nineteen are respiratory + respiratory (16 of them involve COVID-19; e.g. Baycrest Hospital 7E, 2025-01-27, COVID-19 and Influenza A, as two rows). These look like a single co-detection event recorded twice.

<details>
<summary>Full table: <code>Causative Agent-1</code> by year</summary>

|Agent                                        | 2016| 2017| 2018| 2019| 2020| 2021| 2022| 2023| 2024| 2025| 2026| total|
|:--------------------------------------------|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|-----:|
|(CPE) - Colonization (KPC)                   |    0|    0|    0|    2|    0|    0|    0|    0|    0|    0|    0|     2|
|(CPE) - Infection - (KPC)                    |    0|    0|    0|    1|    1|    0|    0|    0|    0|    0|    0|     2|
|(CPE) - Infection - (VIM)                    |    0|    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|     1|
|(CPE) - Unspecified (NDM)                    |    0|    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|     1|
|Adenovirus                                   |    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|COVID-19                                     |    0|    0|    0|    0|  391|  353|  974|  700|  567|  256|  126|  3367|
|CPE                                          |    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    1|     1|
|CPE - Colonization                           |    0|    0|    3|    0|    0|    0|    0|    0|    0|    0|    0|     3|
|CPE Citrobacter freundii (KPC)               |    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|     1|
|CPE Colonization CPE Other (KPC)             |    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    1|     1|
|CPE Colonization CPE Other (VIM)             |    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    1|     1|
|CPE Colonization Citrobacter freundii (KPC)  |    0|    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|     1|
|CPE Colonization E. coli (NDM/OXA-48)        |    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|     1|
|CPE Colonization Enterobacter cloacae (VIM)  |    0|    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|     1|
|CPE Colonization Klebsiella oxytoca (NDM)    |    0|    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|     1|
|CPE Colonization Klebsiella pneumoniae (KPC) |    0|    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|     1|
|CPE Colonization Klebsiella pneumoniae (NDM) |    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    1|     1|
|CPE Enterobacter cloacae (KPC)               |    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|     1|
|CPE Enterobacter unspecified (NDM)           |    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|    0|     1|
|CPE Escherichia coli (OXA-48)                |    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|     1|
|CPE Unspecified (KPC)                        |    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|    0|     1|
|CPE Unspecified (NDM)                        |    0|    0|    0|    0|    0|    0|    1|    0|    0|    0|    0|     1|
|CPE Unspecified (VIM)                        |    0|    0|    0|    0|    0|    0|    1|    0|    1|    0|    0|     2|
|Campylobacter jejuni                         |    0|    0|    0|    0|    0|    1|    0|    0|    0|    0|    0|     1|
|Campylobacter unspecified                    |    0|    0|    0|    0|    0|    1|    0|    0|    0|    0|    0|     1|
|Clostridium difficile                        |    5|    1|    2|    5|    2|    0|    0|    0|    2|    2|    0|    19|
|Clostridium difficile (Toxin A & B)          |    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Clostridium difficile (Toxin B)              |    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Coronavirus                                  |   18|   18|    3|    2|    0|    0|    0|    0|    0|    0|    0|    41|
|Coronavirus*                                 |    0|    0|    0|    0|   16|    6|   13|   32|   45|   73|   29|   214|
|Enterovirus                                  |    0|    0|    0|    0|    0|    1|    0|    6|    4|    3|    1|    15|
|Enterovirus/ Rhinovirus                      |    0|    0|    0|    0|    0|    0|    0|    0|   25|   51|   34|   110|
|Enterovirus/Rhinovirus                       |   15|   15|    8|    9|    1|    3|    9|   15|    0|    0|    0|    75|
|Group B Streptococcal disease (neonatal)     |    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|    0|     1|
|Influenza A ((H1N1)pdm09)                    |   29|    0|    3|   13|   18|    0|    0|    0|    1|    3|    1|    68|
|Influenza A (H1)                             |    0|    0|    0|    4|    2|    0|    0|    0|    5|    9|    3|    23|
|Influenza A (H1N1)                           |    0|    0|    2|    3|    4|    0|    0|    2|    5|   27|    4|    47|
|Influenza A (H3)                             |   21|   96|   81|   45|    1|    0|   16|    3|    1|   37|   12|   313|
|Influenza A (H3), Parainfluenza type 1       |    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Influenza A (H3), Parainfluenza type 2       |    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Influenza A (H3N2)                           |   10|   24|   14|    5|    0|    0|    2|    0|    0|    1|    0|    56|
|Influenza A (Not subtyped)                   |    7|   11|   27|   37|   14|    0|   26|   29|   75|  175|   40|   441|
|Influenza A and B                            |    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Influenza B                                  |   17|   14|   91|    0|    3|    0|    0|    0|    3|    0|    0|   128|
|Legionella pneumophila                       |    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    1|     1|
|Metapneumovirus                              |   15|    9|   12|   16|   17|    1|    6|   25|   30|   36|   32|   199|
|Norovirus                                    |    0|    0|    0|    0|    0|    0|    0|   18|   32|   50|   20|   120|
|Norovirus-like                               |   22|   38|   47|   25|   13|    1|   12|    0|    0|    0|    0|   158|
|Parainfluenza                                |    0|    0|    0|    0|    6|    0|    2|   29|   60|   57|   30|   184|
|Parainfluenza PIV III                        |    0|    0|    0|    0|    0|    0|    0|    6|    0|    0|    0|     6|
|Parainfluenza Type 1                         |    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Parainfluenza UNSPECIFIED                    |    0|    0|    0|    0|    0|    1|    0|    0|    0|    0|    0|     1|
|Parainfluenza type 1                         |    4|    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|     5|
|Parainfluenza type 2                         |    0|    2|    0|    1|    0|    0|    0|    0|    0|    0|    0|     3|
|Parainfluenza type 3                         |   10|   10|    8|    6|    0|    0|    0|    0|    2|    1|    1|    38|
|Parainfluenza type 4                         |    0|    4|    0|    0|    0|    0|    0|    0|    0|    0|    1|     5|
|Parainfluenza virus                          |    0|    0|    0|    2|    0|    0|    0|    0|    0|    0|    0|     2|
|Pending                                      |    0|    0|    0|    0|    0|    1|    0|    0|    0|    0|    1|     2|
|Respiratory syncytial virus                  |   32|   30|   11|   34|   19|    2|   30|   50|   30|   43|   69|   350|
|Rhinovirus                                   |   35|   39|    0|    4|    7|   15|   16|   77|   60|   68|   59|   380|
|Rhinovirus, Coronavirus                      |    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Rotavirus                                    |    0|    0|    0|    0|    1|    0|    0|    0|    0|    1|    1|     3|
|Salmonella Kentucky                          |    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|     1|
|Shigella flexneri (Group B)                  |    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Streptococcus pyogenes                       |    2|    0|    1|    0|    0|    0|    0|    1|    1|    0|    1|     6|
|Unable to identify                           |   25|   56|  122|  100|   37|    8|   25|   70|   79|   82|   62|   666|
</details>

<details>
<summary>Full table: <code>Causative Agent-2</code> by year</summary>

|Agent                                                                                                  | 2016| 2017| 2018| 2019| 2020| 2021| 2022| 2023| 2024| 2025| 2026| total|
|:------------------------------------------------------------------------------------------------------|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|-----:|
|<blank>                                                                                                |  235|  305|  396|  295|  532|  393| 1122|    0|  994|  965|  530|  5767|
|Adenovirus                                                                                             |    0|    0|    0|    0|    1|    0|    0|    0|    0|    0|    0|     1|
|Adenovirus, Metapneumovirus                                                                            |    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|CPE - Infection                                                                                        |    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|CPE Colonization E. coli (NDM), CPE Infection E.coli (NDM/OXA-48), CPE Infection E. coli (OXA-48), VRE |    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|     1|
|CPE Escherichia coli (NDM), CPE Escherichia coli (OXA-48), VRE                                         |    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|     1|
|CPE Infection E.coli (NDM), VRE                                                                        |    0|    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|     1|
|CPE Infection Klebsiella pneumoniae (NDM), CPE Colonization E.coli (NDM)                               |    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|    1|     1|
|CPE Other (KPC)                                                                                        |    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|    0|     1|
|Clostridium difficile                                                                                  |    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|     1|
|Coronavirus                                                                                            |    9|   17|    2|    0|    0|    0|    0|    0|    0|    0|    0|    28|
|Coronavirus*                                                                                           |    0|    0|    0|    0|    4|    0|    0|    2|    8|    2|    0|    16|
|Coronavirus*, Enterovirus/ Rhinovirus                                                                  |    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|     1|
|Coronavirus*, Rhinovirus                                                                               |    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|    0|     1|
|Coronavirus, Enterovirus                                                                               |    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Coronavirus, Respiratory syncytial virus                                                               |    1|    3|    0|    0|    0|    0|    0|    0|    0|    0|    0|     4|
|Coronavirus, Rhinovirus                                                                                |    2|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|     2|
|Enterovirus                                                                                            |    0|    0|    0|    0|    1|    0|    0|    2|    0|    0|    0|     3|
|Enterovirus, Metapneumovirus                                                                           |    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Enterovirus, Rhinovirus                                                                                |    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Enterovirus/ Rhinovirus                                                                                |    0|    0|    0|    0|    0|    0|    0|    1|    3|    0|    0|     4|
|Enterovirus/Rhinovirus                                                                                 |    1|    1|    3|    1|    0|    0|    1|    2|    0|    0|    0|     9|
|Influenza A ((H1N1)pdm09), Influenza A (H3)                                                            |    0|    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|     1|
|Influenza A (H1)                                                                                       |    0|    0|    0|    1|    2|    0|    0|    0|    0|    0|    0|     3|
|Influenza A (H1N1)                                                                                     |    0|    0|    0|    0|    0|    0|    0|    0|    0|    2|    0|     2|
|Influenza A (H1N1), Respiratory syncytial virus                                                        |    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|    0|     1|
|Influenza A (H3)                                                                                       |    0|    0|    0|    0|    0|    0|    0|    0|    0|    2|    0|     2|
|Influenza A (Not subtyped)                                                                             |    0|    0|    0|    0|    1|    0|    3|    3|    2|    2|    0|    11|
|Influenza B                                                                                            |    1|    2|   13|    0|    0|    0|    0|    0|    0|    0|    0|    16|
|Influenza B, Coronavirus*                                                                              |    0|    0|    0|    0|    1|    0|    0|    0|    0|    0|    0|     1|
|Influenza B, Metapneumovirus                                                                           |    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Influenza B,Respiratory syncytial virus                                                                |    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Metapneumovirus                                                                                        |    3|    4|    4|    3|    3|    0|    0|    6|    2|    0|    0|    25|
|Metapneumovirus, Coronavirus                                                                           |    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Metapneumovirus, Coronavirus*                                                                          |    0|    0|    0|    0|    1|    0|    0|    0|    0|    0|    0|     1|
|Metapneumovirus, Parainfluenza                                                                         |    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|     1|
|Metapneumovirus, RSV                                                                                   |    0|    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|     1|
|Metapneumovirus, Respiratory syncytial virus                                                           |    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Metapneumovirus, Rhinovirus                                                                            |    0|    0|    0|    1|    1|    0|    1|    0|    0|    0|    0|     3|
|None                                                                                                   |    0|    0|    0|    0|    0|    0|    0| 1018|    0|    0|    0|  1018|
|Norovirus-like                                                                                         |    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Parainfluenza                                                                                          |    0|    0|    0|    1|    0|    0|    3|    4|    8|    1|    0|    17|
|Parainfluenza type 1                                                                                   |    0|    3|    1|    1|    0|    0|    0|    0|    1|    0|    0|     6|
|Parainfluenza type 2                                                                                   |    0|    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|     1|
|Parainfluenza type 2 & type 4                                                                          |    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Parainfluenza type 3                                                                                   |    1|    1|    0|    1|    0|    0|    0|    0|    0|    0|    0|     3|
|Parainfluenza type 3,  Respiratory syncytial virus                                                     |    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Parainfluenza type 3, RSV                                                                              |    0|    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|     1|
|Parainfluenza type 4                                                                                   |    0|    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Parainfluenza virus                                                                                    |    1|    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|     2|
|Parainfluenza, Coronavirus*                                                                            |    0|    0|    0|    0|    1|    0|    0|    0|    0|    0|    0|     1|
|Parainfluenza, Metapneumovirus                                                                         |    0|    0|    0|    1|    0|    0|    0|    0|    0|    0|    0|     1|
|Parainfluenza, Rhinovirus                                                                              |    0|    0|    0|    0|    0|    0|    0|    0|    1|    0|    0|     1|
|Respiratory syncytial virus                                                                            |    9|   16|   16|    9|    2|    0|    0|    9|    3|    0|    0|    64|
|Respiratory syncytial virus/ C                                                                         |    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|     1|
|Rhinovirus                                                                                             |    2|   10|    0|    0|    3|    1|    1|   16|    6|    3|    1|    43|
|Rhinovirus, Respiratory syncytial virus                                                                |    0|    0|    0|    0|    0|    0|    2|    0|    0|    0|    0|     2|
|Rhinovirus/Respiratory syncytial virus                                                                 |    1|    0|    0|    0|    0|    0|    0|    0|    0|    0|    0|     1|
</details>

<details>
<summary>COVID-19 vs seasonal coronavirus labels by onset month, Dec 2019 – Jun 2020</summary>

|onset_month | agent1: Coronavirus| agent2 only| agent1: Coronavirus*| agent1: COVID-19|
|:-----------|-------------------:|-----------:|--------------------:|----------------:|
|2019-12     |                   2|           0|                    0|                0|
|2020-01     |                   0|           5|                    6|                0|
|2020-02     |                   0|           0|                    1|                0|
|2020-03     |                   0|           1|                    7|               26|
|2020-04     |                   0|           0|                    2|               82|
|2020-05     |                   0|           0|                    0|               43|
|2020-06     |                   0|           0|                    0|               24|
</details>

## 5. Proposed harmonization scheme (for Phase 2)

### Agents
Before mapping, split each agent field into its individual agents. The separators are `,`, ` and `, `&`, and `/` except inside `Enterovirus/ Rhinovirus`. Blank values and `None` mean no agent. Then map each agent to one analysis group:

| Analysis group | Raw labels |
|---|---|
| influenza A | every `Influenza A (…)` label; the A half of `Influenza A and B` |
| influenza B | `Influenza B`; the B half of `Influenza A and B` |
| RSV | `Respiratory syncytial virus`, `RSV`, and the truncated `Respiratory syncytial virus/ C` |
| COVID-19 | `COVID-19` |
| rhinovirus/enterovirus | `Rhinovirus`, `Enterovirus`, `Enterovirus/Rhinovirus`, `Enterovirus/ Rhinovirus` |
| seasonal coronavirus | `Coronavirus` (2016–19), `Coronavirus*` (2020+) |
| parainfluenza | every `Parainfluenza …` variant |
| hMPV | `Metapneumovirus` |
| norovirus | `Norovirus`, `Norovirus-like` (keeping a flag for the label change) |
| other | Adenovirus, *C. difficile* (3 variants), CPE (all variants), *Campylobacter*, *Salmonella*, *Shigella*, Rotavirus, *S. pyogenes*, Group B strep, *Legionella* |
| unknown | `Unable to identify`, `Pending` |

New columns per outbreak: `agent1_raw`, `agent2_raw`, `agent_group_primary` (the first agent listed in Agent-1), `agent_groups_all` (a list or pipe-separated string), `n_agents`, and `co_detection` (TRUE when more than one agent is listed in either field). The mapping lives in a lookup CSV (`data/reference/agent_lookup.csv`) that a reviewer can read. Any raw label not in the lookup fails loudly, so new spellings get triaged rather than silently landing in "other".

### Setting
- `setting_group`: hospital / LTCH / retirement home, plus a decision on Shelter (Q1).
- `hospital_subtype`: acute care / chronic care / psychiatric / transitional care.

### Facility
Stripping the last `" - …"` segment from the name is not reliable on its own:
- Parent names often contain `" - "` themselves (`William Osler Health System - Etobicoke General Hospital - 7E`).
- Names were truncated at 60 characters until 2021.
- The same site is spelled many ways (St. Joseph's appears under at least five names at 30 The Queensway).

I propose:
1. Build the **facility key from the normalized address**: uppercase, strip `" - unit"` suffixes, standardize street types and directions, remove punctuation. This alone reduces 628 raw addresses to about 360.
2. Add a **hand-curated crosswalk** (`data/reference/facility_crosswalk.csv`) for what normalization can't fix:
   - typos (`5395`/`5935 Bathurst`, `Ressurection Rd`);
   - multi-number campuses (`700 & 720 Mount Pleasant`);
   - shared addresses with several operators (200 Church St and 2111 Finch Ave W Reactivation Care Centres; 1001 Queen St W for CAMH).
3. Give each facility a display name: its most frequent name stem in recent years.
4. Keep the unit label (the raw `Institution Name`) and a unit key (normalized name + facility key).

### Dates, duration, season
- `duration_days = declared_over − began`. Open outbreaks are censored (`event = 0`) at the **data-as-of date** (the Wednesday before the refresh; here 2026-11-11), not the download date. The 0-day outbreak is kept.
- `season` = ISO week 35 to week 34 (e.g. `2025/26`), `epi_week` = ISO week, plus `season_week` (1–52/53, counted from week 35).
- 2015/16 is a partial season: the data start in January 2016.

### Deduplication
There are no cross-file duplicates now, but I'd keep an exact-match safeguard (all fields except `_id`) with a test, in case the City changes how rows are filed. The same-unit, same-onset pairs are a separate decision (Q3).

### Note for Phase 3 (snapshots)
Because `_id` restarts in every file, measuring reporting delay and retrospective edits across snapshots will need a **composite key** (unit name + address + onset date + Agent-1). That key changes whenever the City edits one of those fields, e.g. when an agent goes from `Pending` to a pathogen. The snapshot comparison will need fuzzy matching on unit and onset, with explicit rules for edits.

## 6. Decisions I need from you before Phase 2

1. **Shelter and Transitional Care.** My proposal: Transitional Care → hospital (subtype "transitional care"), because these are hospital-operated units. Shelter (3 rows, 2016–18) → exclude from the analysis datasets, but keep it in the cleaned file with a flag.
2. **Norovirus-like → norovirus.** Merge them, with a flag and a note in Methods? (My proposal: yes.)
3. **Same unit, same onset, different respiratory agent (19 pairs).** Count as (a) two outbreaks, as published, or (b) one outbreak with a co-detection? My proposal: (a) for headline counts, to stay consistent with what TPH publishes, plus a sensitivity count under (b) and a note in Methods.
4. **Counting by agent when there are co-detections.** For agent-specific series (agent mix, onset alerts), count an outbreak (a) once, under its primary (first-listed) agent, or (b) once under every agent detected? My proposal: (b) for agent-specific series, which gives each pathogen's outbreak activity, and (a) for the stacked agent-mix chart so the bars sum to the total. COVID is always listed first, which would bias (a) toward COVID.
5. **Facility definition.** A physical site (the address-based key, split by setting where one address holds both an LTCH and a retirement home), not the operator or hospital network? (My proposal: yes.)
6. **Pre-pandemic baseline window.** The 2019/20 season was disrupted from about ISO week 11 of 2020. Use 2016/17–2018/19 as complete seasons and include 2019/20 only up to week 10? Or drop 2019/20 entirely? Also, 2015/16 is partial and would be excluded.
7. **Week 53** (2015/16 and 2020/21 seasons). For the season-on-season charts, merge week 53 into week 52? (My proposal: yes, and say so in Methods.)

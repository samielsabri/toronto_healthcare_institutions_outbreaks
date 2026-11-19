# Methodological decisions log

Decisions made after reviewing the Phase 1 EDA ([`01_eda.md`](01_eda.md), section 6).
Evidence for each is reproducible with [`scripts/decision_aids.R`](../scripts/decision_aids.R).

| # | Question | Decision | Rationale |
|---|---|---|---|
| D1 | Where do the "Shelter" and "Transitional Care" settings go? | **Transitional Care → hospital** (subtype "transitional care"). **Shelter → kept in the cleaned data but flagged out of scope** and excluded from analyses. | Transitional care units are hospital-operated; the same units at 200 Church St are coded "Hospital-Chronic Care" in other years, so the label looks like coding drift. Shelter is 3 Seaton House outbreaks (2016–18) and is not a healthcare institution. |
| D2 | Merge "Norovirus-like" (to 2022) with "Norovirus" (2023 on)? | **Merge** into the norovirus group. The raw label is retained. | The label switches cleanly between years and durations are similar. Methods will note that "Unable to identify" rose from 48% to 62% of enteric outbreaks at the switch, so the label change may also reflect a change in what was called norovirus. |
| D3 | Same unit, same onset date, different respiratory agent (19 pairs): one outbreak or two? | **Keep as published** (two outbreaks). The pairs are flagged in the cleaned data. | Matches the source and TPH's published counts; the effect is at most about 1% of rows in any year. Methods will note that co-detections were more often recorded within one row before 2020 and as separate rows after. |
| D4 | With co-detections, which agent does an outbreak count under? | **Every agent detected** for per-agent series and onset alerts. The stacked agent-mix chart shows co-detections as a separate **"multiple agents"** band so each outbreak is counted once. | Per-agent series answer "how many outbreaks involved this pathogen?". Counting by first-listed agent only loses over a third of RSV outbreaks in some pre-2020 seasons, and the field order is arbitrary (COVID-19 is always listed first). |
| D5 | What is a "facility"? | **Physical site** (normalized address), **split by setting** where one address holds more than one setting (e.g. an LTCH and a retirement home). Not the operator. The unit-level label is kept too. | Recurrence is about a building, its residents and its staff. Operator names change and merge over time. Co-located LTCHs and retirement homes are separately licensed with separate residents. |
| D6 | Pre-pandemic baseline window | **2016/17, 2017/18, 2018/19, plus 2019/20 up to ISO week 10 of 2020** (season week 28). 2015/16 excluded (partial: data start January 2016). | 2019/20 tracks earlier seasons until week 10, then shows a testing spike (weeks 11–12) and a collapse in non-COVID outbreaks. Methods will note that the baseline rests on 3–4 seasons. |
| D7 | ISO week 53 | **Merge week 53 into week 52** for season-on-season charts. | Keeps every season on one 52-week axis; affects 2015/16 (1 outbreak) and 2020/21 (24), both outside the baselines. |

## Decisions carried over from the Phase 1 proposal (not objected to)

- **Censoring date** for open outbreaks: the data-as-of date (the Wednesday before the Thursday refresh), not the download date.
- **Agent mapping** lives in a reviewable lookup table (`data/reference/agent_lookup.csv`); an unmapped raw label stops the pipeline.
- **Deduplication** safeguard on all fields except `_id`, although no cross-file duplicates exist in the current extract.

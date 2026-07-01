# Japanese Revision Plan for Coffee and Research

## 1. Overall Diagnosis

The Japanese site already has a strong identity: a father and daughter talk through clinical research, statistics, causality, R, and scientific writing over coffee. The article sequence is broad and technically useful, and many pages already connect explanation, figures, code, and references.

The main weakness is not lack of content but uneven reader guidance. Some articles work well when read in sequence, but readers arriving from search may need a clearer local purpose, a short reminder of key definitions, and a bridge to related pages. The new book manuscript offers useful conceptual framing, especially around language, denominators, p-values, AI-assisted analysis, confounding, causal models, and paper writing. These ideas should be adapted to the article body without importing the grandmother setting or long literary scenes.

The revision should therefore keep the existing father-version dialogue and strengthen the site as a searchable, referable educational resource.

## 2. Recommended Positioning

The top page should keep its original positioning and tone. The book manuscript should be used only as a behind-the-scenes reference for improving article bodies.

Working positioning for article revision:

> 新書原稿は、説明の明確化、用語の整理、概念のつなぎ方を考えるための参照資料として使う。ブログ本文では、父と娘の会話設定を保ち、図・Rコード・補足説明・関連リンクを活かして、個別記事をより読みやすく、参照しやすくする。

## 3. Priority Revision List

| Priority | Target file | Target section | Related book topic | Current issue | Revision policy | Proposed Japanese text or outline | Type | Risk or caution |
|---|---|---|---|---|---|---|---|---|
| High | `jp/study-design-1.qmd` | Research hypothesis | Chapter 1 | PICO/PECO is explained, but denominator and comparison could be foregrounded | Add web note about question, denominator, comparison | 「パーセントは分子より先に分母で性格が決まる」 adapted | Conceptual | Avoid copying book prose |
| Medium | `jp/study-design-3.qmd` | Outcome | Chapter 1 | Outcome definition is central but can be connected to analysis plan | Add caution note about outcome as bridge | Outcome definition determines table, model, interpretation | Conceptual | Keep local flow |
| Medium | `jp/study-design-5.qmd` | Bias | Chapter 1/7 | Bias categories exist but reader may miss design-before-analysis point | Add note: bias enters before model | Bias often cannot be repaired after data collection | Conceptual | Avoid fatalistic tone |
| High | `jp/frequentist-2.qmd` | p-value misconceptions | Chapters 4-5 | Strong discussion, but needs a compact "wrong readings" box | Add local summary | p-value is not probability null is true; not importance; not proof of equality | Conceptual/wording | Preserve frequentist meaning |
| High | `jp/frequentist-3.qmd` | p-value interpretation | Chapters 4-5 | Needs link from p-value to Methods | Add note: p-value depends on design and analysis rule | Read with null hypothesis, endpoint, test, alpha, multiplicity | Conceptual | Avoid overloading |
| Medium | `jp/frequentist-5.qmd` | Sample size | Chapter 5 | Alpha/beta/power could be tied to error planning | Add short note | Sample size design is study error planning | Conceptual | No formula expansion |
| High | `jp/ai-r-1.qmd` | AI-assisted workflow | Chapter 6 | Good practical workflow, but AI boundary could be sharper | Add "AIに任せる/任せない" note | AI may write code; researcher owns data definition, estimand, QC | Conceptual/QC | Avoid tool-specific promises |
| High | `jp/ai-r-3.qmd` | Estimand/QC | Chapters 6-7 | Strong but long; needs overview before code | Add reader guidance | Start from estimand and assumptions, not plot output | Structural | Avoid duplicate explanation |
| High | `jp/logistic-regression-6.qmd` | Confounding | Chapter 7 | Good example, but common cause vs effect measure needs sharper distinction | Add note: adjustment changes comparison target | Confounding is broken comparison; non-collapsibility is effect measure property | Conceptual | Must be technically accurate |
| Medium | `jp/logistic-regression-4.qmd` | Table/results | Chapter 6 | Table interpretation and QC connect to AI/R | Add link to AI & R | Table production is part of analysis plan | Navigation | Keep article focus |
| High | `jp/causal-inference-4.qmd` | Causal frameworks | Chapter 8 | Strong metaphor; estimand link can be clearer | Add note on estimand | DAG/probability model/potential outcomes answer different layers of the same question | Conceptual | Do not overstate equivalence |
| Medium | `jp/causal-inference-5.qmd` | Backdoor paths | Chapter 8 | Blocking is explained but reader may need purpose | Add note: adjustment set is not a list of available variables | Choose variables from causal structure | Conceptual | Avoid causal cookbook tone |
| High | `jp/publish-a-paper-2.qmd` | Results/Discussion/Limitations | Chapter 9 | Good writing advice; could better state section roles | Add web note about Results, Discussion, Limitations | Results reports what was observed; Discussion interprets; Limitations returns numbers to words | Structural/wording | Avoid making it a writing manual replacement |
| Medium | `jp/glossary.qmd` | Terms | Appendix | Useful but can become a hub | Add links to key pages after selected terms later | p-value, risk, odds, confounding, estimand | Navigation | Larger glossary work should be reviewed separately |
| Low | `_quarto.yml` | Navigation | Whole site | Some labels are English or typo-prone | Later harmonize navbar labels | Adjust "Adjsuted" typo and add clearer JP labels | Navigation | Shared config affects both languages |

## 4. Top Page Policy

Do not revise the top page to explain the relationship with the book. Keep the original top-page language and use the manuscript only for article-body improvements.

## 5. Article-Level Templates

Use these elements where they help, without forcing every article into the same mold.

- `この記事で考えること`: The local question and why it matters.
- `まず押さえること`: One to three definitions needed before reading.
- `会話`: The existing father-daughter dialogue remains the core.
- `研究・論文でのポイント`: How the concept appears in actual clinical research.
- `Rコードまたは図`: Reproducible script, output, or figure.
- `よくある読み違い`: Common misinterpretations, especially for p-values, percentages, odds ratios, confounding, and estimands.
- `関連エピソード`: Links to prerequisite and next pages.
- `新書原稿から得た改善点`: Use internally as an editorial reference; do not add an explicit book-positioning section to the top page.

## 6. Do-Not-Change List

- Do not change the father to a grandmother.
- Do not import large literary or domestic scenes from the book.
- Do not rewrite the blog into the book style.
- Do not copy long passages from the manuscript.
- Do not simplify technical concepts in a way that makes them inaccurate.
- Do not make the blog a full substitute for the book.
- Do not edit the English version before the Japanese changes are defined.
- Do not make large automated rewrites without review.
- Do not remove existing figures, code links, or reproducibility material unless they are broken and intentionally replaced.

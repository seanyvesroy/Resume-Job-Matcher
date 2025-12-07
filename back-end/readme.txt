Resume–Job Matcher (Knowledge Representation & Reasoning Backend)

This project is a Logic Programming + Knowledge Graph–based resume/job matching system built using:

- SWI-Prolog for reasoning and temporal logic
- Python + FastAPI for the web backend
- pyswip as the Python ↔ Prolog bridge

It computes an explainable semantic + temporal match score between resume skills and job requirements.

=====================================================================
REQUIREMENTS
=====================================================================

You must have:

- Python 3.9+
- SWI-Prolog 9.x+
- Windows / macOS / Linux supported

---------------------------------------------------------------------
INSTALLING SWI-PROLOG (WINDOWS)
---------------------------------------------------------------------

1. Download from:
   https://www.swi-prolog.org/download/stable

2. Install the 64-bit version

3. DURING INSTALL:
   CHECK: “Add swipl to PATH”

4. Restart PowerShell

Verify installation:

swipl --version

Expected output:
SWI-Prolog version 9.x.x for x64-win64

---------------------------------------------------------------------
INSTALLING SWI-PROLOG (MAC)
---------------------------------------------------------------------

brew install swi-prolog

---------------------------------------------------------------------
INSTALLING SWI-PROLOG (LINUX)
---------------------------------------------------------------------

sudo apt install swi-prolog

=====================================================================
INSTALLING PYTHON DEPENDENCIES
=====================================================================

From the project root:

pip install fastapi uvicorn pyswip pydantic

If pyswip fails, make sure:
swipl --version works first.

=====================================================================
PROJECT STRUCTURE
=====================================================================

project/
|
|-- app.py
|-- likeness_scoring.pl
|-- README.txt

=====================================================================
RUNNING THE BACKEND SERVER
=====================================================================

From the project folder:

uvicorn app:app --reload

You should see:

Uvicorn running on http://127.0.0.1:8000

=====================================================================
OPENING THE API DOCUMENTATION
=====================================================================

Open in your browser:

http://127.0.0.1:8000/docs

This provides a live API tester.

=====================================================================
API ENDPOINT
=====================================================================

POST /score

URL:
http://127.0.0.1:8000/score

=====================================================================
JSON REQUEST FORMAT
=====================================================================

{
  "resume": [
    { "term": "azure",  "start": 201901, "end": 202305 },
    { "term": "csharp", "start": 202103, "end": 202107 },
    { "term": "sql",    "start": 201701, "end": 201812 }
  ],
  "job": [
    { "term": "azure", "importance": 1.0, "start": "none", "end": "none" },
    { "term": ".net",  "importance": 1.0, "start": 201801, "end": "none" },
    { "term": "mssql", "importance": 0.7, "start": "none", "end": "none" }
  ]
}

Field meanings:

term        = skill name
start/end   = YYYYMM format
"none"      = no timeframe requirement
importance  = weight (0.0 – 1.0)

=====================================================================
EXAMPLE API RESPONSE
=====================================================================

{
  "score": 0.84,
  "breakdown": [
    entry(azure,1.0,azure,1.0,1.0,1.0),
    entry(.net,1.0,csharp,0.90,0.90,1.0),
    entry(mssql,0.7,sql,0.63,0.90,1.0)
  ]
}

Each breakdown entry means:

entry(
  JobTerm,
  Importance,
  BestResumeTerm,
  PairScore,
  Similarity,
  TemporalFactor
)

=====================================================================
CALLING FROM A FRONTEND (JAVASCRIPT)
=====================================================================

fetch("http://127.0.0.1:8000/score", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({
    resume: [
      { term: "azure", start: 201901, end: 202305 },
      { term: "csharp", start: 202103, end: 202107 }
    ],
    job: [
      { term: "azure", importance: 1.0, start: "none", end: "none" },
      { term: ".net", importance: 1.0, start: 201801, end: "none" }
    ]
  })
})
.then(res => res.json())
.then(data => console.log(data));

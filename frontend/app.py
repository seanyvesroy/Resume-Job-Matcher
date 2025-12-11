import json
from typing import List, Dict

import streamlit as st
import requests
import pandas as pd

# -------------------------------------------------
# CONFIG
# -------------------------------------------------
BACKEND_URL = "http://127.0.0.1:8000"

st.set_page_config(page_title="Resume–Job Matcher", page_icon="🤝", layout="wide")

# -------------------------------------------------
# SMALL HELPERS
# -------------------------------------------------
def call_parse_resume(resume_text: str) -> Dict:
    files = {
        "file": ("resume.txt", resume_text.encode("utf-8"), "text/plain")
    }
    resp = requests.post(f"{BACKEND_URL}/parse/resume", files=files, timeout=10)
    resp.raise_for_status()
    return resp.json()


def call_parse_job(job_text: str) -> Dict:
    files = {
        "file": ("job.txt", job_text.encode("utf-8"), "text/plain")
    }
    resp = requests.post(f"{BACKEND_URL}/parse/job", files=files, timeout=10)
    resp.raise_for_status()
    return resp.json()


def call_score_from_text(resume_terms: List[Dict], job_terms: List[Dict]) -> Dict:
    payload = {
        "resume": resume_terms,
        "job": job_terms,
    }
    resp = requests.post(f"{BACKEND_URL}/score/from-text", json=payload, timeout=10)
    resp.raise_for_status()
    return resp.json()


# -------------------------------------------------
# SESSION STATE INITIALISATION
# -------------------------------------------------
if "resume_terms" not in st.session_state:
    st.session_state.resume_terms = []

if "job_terms" not in st.session_state:
    st.session_state.job_terms = []


# -------------------------------------------------
# UI LAYOUT
# -------------------------------------------------
st.title("🤝 Resume – Job Matcher")

st.markdown(
    """
    1. Paste your **resume** and **job** in the standard text formats  
    2. Click **“Parse ”**  
    3. Verify or edit the parsed terms  
    4. Click **“Compute final score”**  
    """
)



col_left, col_right = st.columns(2)

# ---------------------- INPUTS ----------------------
with col_left:
    st.subheader("1. Resume text")

    resume_example = """NAME: Sean McCarthy
EMAIL: sean@email.com

SKILLS:
azure
csharp
sql

EXPERIENCE:
azure, 201901, 202305
csharp, 202103, 202107
sql, 201701, 201812
"""

    resume_text = st.text_area(
        "Paste resume in standard format",
        value=resume_example,
        height=250,
    )

with col_right:
    st.subheader("2. Job description text")

    job_example = """JOB_TITLE: Cloud Software Engineer

REQUIREMENTS:
azure, 1.0
.net, 1.0
mssql, 0.7

TIME_REQUIREMENTS:
.net, 201801, none
"""

    job_text = st.text_area(
        "Paste job description in standard format",
        value=job_example,
        height=250,
        
    )
    

st.markdown("---")

# ---------------------- PARSING STEP ----------------------
parse_btn = st.button("🔍 Parse ")

if parse_btn:
    if not resume_text.strip() or not job_text.strip():
        st.error("Please fill both resume and job text before parsing.")
    else:
        try:
            with st.spinner("Calling /parse/resume ..."):
                resume_parsed = call_parse_resume(resume_text)
            with st.spinner("Calling /parse/job ..."):
                job_parsed = call_parse_job(job_text)

            st.session_state.resume_terms = resume_parsed.get("resume_terms", [])
            st.session_state.job_terms = job_parsed.get("job_terms", [])

            st.success("Parsed successfully. Review the extracted terms below.")
        except Exception as e:
            st.error(f"Error while parsing: {e}")

# ---------------------- REVIEW / EDIT PARSED DATA ----------------------
st.subheader("3. Review and edit parsed terms")

col_r1, col_r2 = st.columns(2)

with col_r1:
    st.markdown("**Resume terms**")
    if st.session_state.resume_terms:
        resume_df = pd.DataFrame(st.session_state.resume_terms)
        edited_resume_df = st.data_editor(
            resume_df,
            num_rows="dynamic",
            key="resume_editor",
            use_container_width=True,
        )
    else:
        st.info("No resume terms yet. Parse first.")
        edited_resume_df = None

with col_r2:
    st.markdown("**Job terms**")
    if st.session_state.job_terms:
        job_df = pd.DataFrame(st.session_state.job_terms)
        edited_job_df = st.data_editor(
            job_df,
            num_rows="dynamic",
            key="job_editor",
            use_container_width=True,
        )
    else:
        st.info("No job terms yet. Parse first.")
        edited_job_df = None

st.markdown("---")

# ---------------------- FINAL SCORING STEP ----------------------
st.subheader("4. Compute final score via /score/from-text")

score_btn = st.button("✅ Compute final score")

if score_btn:
    if edited_resume_df is None or edited_job_df is None:
        st.error("You need parsed data first. Click on 'Parse using backend'.")
    else:
        # Convert back to list of dicts
        resume_terms = edited_resume_df.to_dict(orient="records")
        job_terms = edited_job_df.to_dict(orient="records")

        try:
            with st.spinner("Calling /score/from-text ..."):
                result = call_score_from_text(resume_terms, job_terms)

            score = result.get("score", None)
            breakdown = result.get("breakdown", None)

            if score is None:
                st.error("Backend did not return a score.")
            else:
                st.metric("Final match score", f"{score:.2f}")
                st.progress(min(float(score), 1.0))

            st.markdown("#### Raw breakdown from Prolog")
            if breakdown is not None:
                st.code(str(breakdown), language="prolog")
            else:
                st.write("No breakdown returned.")

            st.caption(
                "later parse this breakdown a prettier explanation."
            )

        except Exception as e:
            st.error(f"Error while scoring: {e}")

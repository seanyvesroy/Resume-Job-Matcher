from pyswip import Prolog

# Initialize Prolog
prolog = Prolog()

# Load your Prolog file
prolog.consult("likeness_scoring.pl")

def run_demo_score():
    """
    Calls final_score(Score, Breakdown) from Prolog
    Returns Python dict with results
    """
    query = list(prolog.query("final_score(Score, Breakdown)"))

    if not query:
        return {"error": "No result returned from Prolog"}

    result = query[0]

    score = float(result["Score"])
    breakdown = result["Breakdown"]

    return {
        "score": score,
        "breakdown": breakdown
    }

if __name__ == "__main__":
    output = run_demo_score()
    print("Final Score:", output["score"])
    print("Breakdown:", output["breakdown"])

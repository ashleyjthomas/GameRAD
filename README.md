# Relationship Detective (GameRAD)

An interactive web-based research game for children, designed to explore how children understand different types of social relationships.

## About

Relationship Detective presents children with short illustrated stories about pairs of alien characters and asks them questions about how those characters act toward each other. The game examines whether children can recognize and distinguish between three relationship types from Fiske's Relational Models theory:

- **Communal Sharing (CS)** — pairs who treat everything as shared, feel each other's emotions, and see themselves as one unit
- **Equality Matching (EM)** — pairs who keep close track of fairness, take turns, and make sure things stay even between them
- **Authority Ranking (AR)** — pairs where one character leads and the other follows, with clear differences in rank or power

The game is built as a single self-contained HTML file with pre-recorded audio narration, designed to run unattended in a browser during asynchronous online study sessions hosted on [Lookit / Children Helping Science](https://childrenhelpingscience.com).

## Research context

- **Principal Investigator:** Ashley Thomas, Harvard University
- **Contact:** athomas@g.harvard.edu
- **IRB Approval:** Harvard University Area Institutional Review Board

## How it works

1. A parent completes a video consent form on Lookit / Children Helping Science.
2. A unique participant ID is generated and passed to the game via URL parameter.
3. The child plays through 12 short story-and-question rounds. Each round:
   - Tells a story about two alien characters using audio narration with synchronized highlighted text
   - Asks the child to identify which relationship type the pair has (CS / EM / AR)
   - Follows up with three yes/no "test item" questions
4. Responses are stored locally and automatically uploaded to a Google Sheet for the study database.
5. At the halfway point, children see a celebration screen with a reward video and an option to stop or continue.
6. At the end, children are prompted to find their parent so the rest of the session can be completed.

## Repository structure

```
GameRAD/
├── index.html              Main game file (single-file HTML + CSS + JS)
├── archive/                Older versions of index.html, kept for reference
├── audio_*.mp3, audio_*.m4a   Pre-recorded audio narration
├── *.png                   Character art, scene illustrations, asset images
├── *.mp4                   Reward video clips
├── README.md               This file
├── LICENSE                 CC BY 4.0 license
└── .gitignore              Files Git should ignore
```

## Running locally

This is a static HTML file with no build step. To run locally for development or testing:

```bash
# Clone the repo
git clone https://github.com/ashleyjthomas/GameRAD.git
cd GameRAD

# Serve over a local HTTP server (browsers will not autoplay media from file://)
python3 -m http.server 8000

# Open in your browser
open http://localhost:8000
```

Append URL parameters to simulate a real session:

```
http://localhost:8000/?pid=test_participant&chsid=test_session
```

The game is deployed live via GitHub Pages at https://ashleyjthomas.github.io/GameRAD/.

## Researcher control panel

A hidden researcher panel can be revealed for debugging or skipping ahead during testing. To open it:

- Press **Ctrl + Shift + R**, or
- Tap the bottom-right corner of the screen 5 times within 3 seconds

The panel includes options to skip the current pair, jump to the midpoint, jump to the end screen, and download a CSV of the current participant's responses.

## Data collected

For each participant, the game records:

- Participant ID and timestamp
- Child's first name and age
- For each test item: vignette ID, question ID, congruence with the vignette's relational model, yes/no answer, confidence (definitely / maybe), and whether the answer matched the keyed direction
- For each model-identification trial: which picture the child chose, whether it matched the vignette's true model, and their confidence

All data is uploaded in real time to a Google Sheet via a Google Apps Script endpoint.

## Citation

If you use this game in your research, please cite:

> Thomas, A. (2026). Relationship Detective: An interactive game for assessing children's understanding of relational models. https://github.com/ashleyjthomas/GameRAD

## License

This work is licensed under a Creative Commons Attribution 4.0 International License (CC BY 4.0). You are free to share and adapt the material for any purpose, including commercially, as long as you give appropriate credit. See [LICENSE](LICENSE) for full terms.

## Acknowledgments

This study runs on [Children Helping Science / Lookit](https://childrenhelpingscience.com), an open platform for asynchronous developmental research.

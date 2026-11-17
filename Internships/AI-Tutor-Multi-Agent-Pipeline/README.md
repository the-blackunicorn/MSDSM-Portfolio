# AI Tutor — Multi-Agent Tutoring Pipeline

A 21-agent pipeline that tutors school students adaptively. Built during an AI/ML internship at Xenocipher Pvt. Ltd. and later integrated into the Student Portal of an education ERP.

## Architecture

**13 LLM agents:** Intent Detector, Safety Guard, Affect Detector, Teaching Strategy, Content Generator, Adaptive Question Generator, Motivation Coach, Math Verifier, Assessment Agent, Misconception Detector, Response Composer, Quality Evaluator, Learning Scientist.

**8 code-only agents:** Orchestrator, Student Model, Difficulty Calibrator, Curriculum Map, Prerequisite Check, Memory Agent, Parent Dashboard, Teacher Dashboard.

## Design decisions

- `concept_id` is subject-agnostic, so the same pipeline works across subjects.
- `bloom_level` gates the Math Verifier: it is mandatory at *apply* level and above.
- The Quality Evaluator runs non-blocking after the Response Composer, so it adds no latency to the student.
- The Learning Scientist fires every 5 turns and tunes the pipeline through `pipeline_parameter_overrides`.
- The Parent Dashboard accepts `home_context_input`, which is forwarded to the Student Model and the Affect Detector.
- Hinglish support through a `language_hint` field.

## Guarding against difficulty manipulation

The Student Model raises `anomaly_flags` when a student seems to be gaming the difficulty: `response_time_too_fast_for_difficulty`, `wrong_answer_pattern` (random, systematic or suspiciously fast) and `confidence_mismatch`. These feed three fixes:

1. The Affect Detector checks for latency anomalies.
2. The Misconception Detector flags systematic errors.
3. A difficulty floor is enforced by the Learning Scientist and the Difficulty Calibrator.

## Testing

All 21 agents were implemented and tested. A complete 6-turn simulated session was run end to end. It surfaced two known issues, both noted and deferred: Safety Guard over-sensitivity on Groq, and math-intent misclassification for very short answers.

## Stack

Python, Groq API (switched from OpenAI mid-project), Google Colab. API keys are read from Colab Secrets and are not stored in the notebooks.

## Files

The notebooks are in this folder.

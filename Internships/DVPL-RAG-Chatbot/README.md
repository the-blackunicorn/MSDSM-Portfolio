# DVPL RAG Chatbot

A WhatsApp-style retrieval-augmented chatbot built for the project manager at **Dee Vee Projects Limited**, a large Indian construction company, during a Business Analyst internship. It answers questions from project documents.

## Stack

- **Orchestration:** LangChain
- **Vector store:** ChromaDB
- **Embeddings:** `paraphrase-multilingual-MiniLM-L12-v2`, which handles English and Hindi queries
- **LLM:** Llama 3.3 70B served through Groq
- **Deployment:** Hugging Face Spaces (Docker + Streamlit)

## How it works

Documents are chunked and embedded into ChromaDB. A question is embedded and the most similar chunks are retrieved. The retrieved context and the question go to Llama 3.3 70B, which answers from that context.

## Related work

During the same internship I benchmarked the company's custom ERP against Procore, Oracle Primavera, SAP S/4HANA, Wondersoft/EasyPro and Autodesk Construction Cloud, and proposed 8 prioritized enhancements. These include an EVM layer (CPI/SPI dashboards), a subcontractor payment workflow with TDS and retention, RFI and submittal management, drawing revision control, a mobile DPR with geotagged photos, and a multi-project financial dashboard. The manager accepted the roadmap, and it is being implemented on a government contract.

## Note

Company documents and data are not included in this repo.

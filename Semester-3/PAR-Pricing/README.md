# Package Pricing at Mission Hospital

Individual case analysis in **R** (regression), IIMB case IMB 527, from the PAR course.

**Question:** can a patient's treatment cost be predicted at admission, so the hospital can quote a fixed package price?

## Data

248 patients with full cost records. Costs range from ₹0.46 lakh to ₹8.87 lakh, and most patients are children, so this is largely a paediatric cardiac practice. Costs are right-skewed, so the model works on log(cost).

## Findings

- Coronary bypass cases are the costliest group. Congenital repairs in children are cheaper and more predictable.
- **The biggest cost driver is unknown at admission.** Each extra ICU day adds about 9% to cost and each ward day about 3%. That is useful for explaining a bill but not for quoting one.
- The model the admission desk can use has six inputs, each with a clear effect. It is useful but not precise.
- **A 20–30% buffer turns a coin-flip into a manageable risk.** Quoting exactly the predicted cost loses money on about 4 in 10 patients. A 20% buffer cuts that to about 1 in 5, with an average margin of about 17%.
- Government scheme rates cover only about half the real cost. Each scheme patient treated at those rates is a loss of roughly ₹0.8–1.2 lakh, which is evidence for renegotiation.

## Files

- `Pricing Mission Hospital_Abhishek Dewangan PPT.pptx`: 13-slide presentation
- `Mission_Hospital_Package_Pricing(Abhishek Dewangan).R`: full analysis code

The case dataset belongs to the case publisher and is not included.

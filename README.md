# Shareholders crimes 

Repository contains:
- the code related to all modelling for the study of shareholders crimes
- the prompt used to classify the court rulings and extract some information from them.
- plots demonstrating balance of covariates after the matching and weighting.
 
The prompt includes several examples of the court rulings.

Definition of all variables can be seen in the code 1_regression_models.r (see function setFixest_dict()).

Financial data comes from the Russian Financial Statements Database. See the accompanying paper here
[here](https://www.nature.com/articles/s41597-025-05150-1?error=cookies_not_supported&code=0e09acc2-bb69-4c82-a033-8b5a53a6f2f1) and the Github Repository [here](github.com/irlcode/rfsd?ysclid=mszta3f2le10285344). I preprocessed the data in the unpublished code.

Ownership data comes from the Uniform State Register of Legal Entities (EGRUL). The data was purchased from the Russian Federal Tax Service (official administrator of the EGRUL) by the institution of the owner of this repository (will become unhidden after review of the paper on shareholders crimes).

Metadata on the corporate litigation and some of the court rulings was granted to the institution of the owner of this repository by private company Pravo.ru, the official administrator the Commercial Case File Database (KAD). We are not allowed to share it.



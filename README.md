# Code for the welfare simulation in: *Do women really comply more? Experimental evidence from a representative sample*

`generate_simulation.ipynb` contains the code for the simulation of the results with different samples.
You will need [Python](https://www.python.org/) and [Jupyter](https://www.jupyter.org/) to run it. 
The simulations rely on the data in `experiment1_long.dta`. The data file should be located in the same folder as the .ipynb file before running the code.

Please refer to the paper for further information.

## Installing the requirements

### Windows/Mac

```bash
pip3 install -r requirements.txt
```

### Linux

```bash
python3 -m venv venv
venv/bin/python3 -m pip install -r requirements.txt
```

## Run the Code

First open a Terminal and navigate to the directory with the code using cd, then run the commands for your OS and open the notebook.

### Windows

```bash
python -m jupyterlab
```

### Mac

```bash
python3 -m jupyter lab
```

### Linux

```bash
venv/bin/jupyter lab
```

## Further instructions

The code is made to run with Jupyter notebooks (see https://docs.jupyter.org/en/latest/running.html), which will be installed (if necessary) along the other requirements in `requirements.txt`. 
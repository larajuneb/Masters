class Plant:
    def __init__(self, barcode, treatment, greenhouse_set) -> None:
        self.barcode = barcode
        self.treatment = treatment
        self.greenhouse_set = greenhouse_set
        self.stem_diameter = {} #{reading #: stem diameter}
        self.num_leaves = {} #{reading #: # leaves}
        self.shoot_length = {} #{reading #: shoot length}
        self.tracked_leaves = [] #type Leaf, to track leaf dimensions
        self.INL_shoot = {} #{reading #: [list of #'s from shoot tip to base]}

    def __str__(self):
        return f"{self.barcode}{self.greenhouse_set} ({self.treatment})"   

class Leaf: #to track leaf dimensions
    def __init__(self, number, selection) -> None:
        self.number = number
        self.selection = selection
        self.growth = {} #{reading #: [width, length]}
import apport.packaging

def add_info(report, ui):
    # add all relevant info like xorg ones
    report.add_hooks_info(ui, srcpackage='xorg')
    

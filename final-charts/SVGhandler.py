import os
import shutil 
import pandas as pd
from lxml import etree as ET

class svg:
    """
    A class to read an SVG object and add interactivity to its tooltips.

    Attributes:
    -----------
    id     : (str) The identifier of the SVG object.
    type   : (str) The figure type of the SVG object (Map, Dumbells, Bars, Lollipop, Dots).
    report : (str) The report identifier derived from the first two characters of the id.

    Methods:
    --------
    addInteractivity(unique_colors):
        Adds interactivity to the SVG object.
    """

    def __init__(self, id, type):
        self.id     = id
        self.type   = type
        self.report = id[:2] 
        
    def addInteractivity(self, unique_colors):

        path2SP = f"/Users/{os.getlogin()}/OneDrive - World Justice Project/EU Subnational/EU-S Data/reports/eu-thematic-reports"
        source = f"{path2SP}/data-viz/output/charts/{self.report}/{self.id}.svg"
        target = f"{path2SP}/final-charts/{self.report}/{self.id}.svg"

        if self.type in ["Table"]:

            print(f"No SVG handler method for {self.id} - type: {self.type}. Saving SVG object to target")
            shutil.copy(source, target)
        
        else:

            print(f"Adding interactivity to chart {self.id}: {self.type}")

            # Reading the SVG file
            tree = ET.parse(source)
            root = tree.getroot()
            root.set("onload", "init(event)")

            # Register namespaces to handle the SVG namespace properly (lxml)
            namespaces = {
                'svg'  : 'http://www.w3.org/2000/svg', 
                'xlink': 'http://www.w3.org/1999/xlink'
            }
            for prefix, uri in namespaces.items():
                ET.register_namespace(prefix, uri)

            # Special process for dumbbells
            if self.type in ["Dumbbells", "Lollipop", "Scatterplot"]:
                
                # Defining unique colors
                if self.type in ["Dumbbells", "Scatterplot"]:
                    region_borders = dict(zip(unique_colors["nuts_id"], unique_colors["border"]))
                    region_labels  = dict(zip(unique_colors["nuts_id"], unique_colors["label"]))
                if self.type in ["Lollipop"]:
                    region_borders = dict(zip(unique_colors["nuts_id"], unique_colors["unique_border"]))
                    region_labels  = dict(zip(unique_colors["nuts_id"], unique_colors["unique_label"]))

                # Creating IDs for circles
                for nuts, color_code in region_borders.items():
                    for element in root.xpath(".//*[@style]", namespaces = namespaces):
                        if color_code in element.attrib["style"]:
                            element.set("id", f"{nuts}_circle")

                # Bagging tooltips elements into a single regional <g> tag
                for nuts, color_code in region_labels.items():
                    nuts_group = ET.Element("g", id = f"{nuts}_tooltip")
                    first_found = False  

                    for element in root.xpath(".//*[@style]", namespaces = namespaces):

                        if color_code in element.attrib["style"]:

                            original_parent = element.getparent()
                            original_parent.remove(element)

                            if not first_found:
                                original_parent.append(nuts_group)
                                first_found = True

                            nuts_group_element = original_parent.find(f".//g[@id='{nuts}_tooltip']")
                            nuts_group_element.append(element)
                
                # Hide tooltips by default
                missing_tooltips = []
                for nuts in unique_colors["nuts_id"].to_list():
                    tooltip = root.find(f".//g[@id='{nuts}_tooltip']")
                    if tooltip is not None:
                        tooltip.set("visibility", "hidden")
                    else:
                        missing_tooltips.append(nuts)
                
                # Assign onmouseover and onmouseout callbacks to patches.
                missing_circles = []
                for nuts in unique_colors["nuts_id"].to_list():
                    circle = root.xpath(f"//svg:circle[@id='{nuts}_circle']", namespaces=namespaces)
                    if circle:
                        circle[0].set("onmouseover", "ShowTooltip(this)")
                        circle[0].set("onmouseout", "HideTooltip(this)")
                    else:
                        missing_circles.append(nuts)
                
                if missing_tooltips or missing_circles:
                    if len(missing_tooltips) == len(missing_circles):
                        print("Data for the following regions not found: " + ", ".join(missing_tooltips))
                    else:
                        print("Tooltips for the following regions not found: " + ", ".join(missing_tooltips))                
                        print("Circles for the following regions not found: " + ", ".join(missing_circles))
                
            # Special process for Maps
            if self.type in ["Map", "Map (Categorical)"]:

                # Defining unique colors
                region_borders = dict(zip(unique_colors["nuts_id"], unique_colors["border"]))
                region_labels  = dict(zip(unique_colors["nuts_id"], unique_colors["label"]))

                # Bagging polygons elements into a single regional <g> tag
                for nuts, color_code in region_borders.items():

                    nuts_group = ET.Element("g", id = f"{nuts}_pols")
                    first_found = False  

                    for element in root.xpath(".//*[@style]", namespaces = namespaces):

                        if color_code in element.attrib["style"]:
                            
                            original_parent = element.getparent()
                            original_parent.remove(element)

                            if not first_found:
                                original_parent.append(nuts_group)
                                first_found = True

                            nuts_group_element = root.find(f".//g[@id='{nuts}_pols']")
                            if nuts_group_element is None:
                                print(f"Data not found for region {nuts}")
                            else:
                                nuts_group_element.append(element)
                
                # Moving coutry polygons at the end for viz purposes
                for element in root.xpath(".//*[@style]", namespaces = namespaces):
                    if "#404040" in element.attrib["style"]:
                        original_parent = element.getparent()
                        original_parent.remove(element)
                        original_parent.append(element)
            
                # Bagging tooltip elements into a single regional <g> tag
                for nuts, color_code in region_labels.items():

                    nuts_group = ET.Element("g", id = f"{nuts}_tooltip")

                    first_found = False  
                    for element in root.xpath(".//*[@style]", namespaces = namespaces):

                        if color_code in element.attrib["style"]:
                            
                            original_parent = element.getparent()
                            original_parent.remove(element)

                            if not first_found:
                                original_parent.append(nuts_group)
                                first_found = True

                            nuts_group_element = root.find(f".//g[@id='{nuts}_tooltip']")
                            if nuts_group_element is None:
                                print(f"Data not found for region {nuts}")
                            else:
                                nuts_group_element.append(element)
                
                # Moving inset tooltips at the end of root for viz purposes
                for nuts in ["CY0", "PT2", "ES7", "PT3"]:
                    inset_tool_bag = root.find(f".//g[@id='{nuts}_tooltip']")
                    if inset_tool_bag is None:
                        print(f"Data not found for inset region {nuts}")
                    else:
                        bag_parent = inset_tool_bag.getparent()
                        root.remove(bag_parent)
                        root.append(bag_parent)
                
                # Hide tooltips by default
                missing_tooltips = []
                for nuts in unique_colors["nuts_id"].to_list():
                    tooltip = root.find(f".//g[@id='{nuts}_tooltip']")
                    if tooltip is not None:
                        tooltip.set('visibility', 'hidden')
                    else:
                        missing_tooltips.append(nuts)

                # Assign onmouseover and onmouseout callbacks to patches.
                missing_polygons = []
                for nuts in unique_colors["nuts_id"].to_list():
                    polygons = root.find(f".//g[@id='{nuts}_pols']")
                    if polygons is not None:
                        polygons.set('onmouseover', "ShowTooltip(this)")
                        polygons.set('onmouseout', "HideTooltip(this)")
                    else:
                        missing_polygons.append(nuts)

                if missing_tooltips or missing_polygons:
                    if len(missing_tooltips) == len(missing_polygons):
                        print("Data for the following regions not found: " + ", ".join(missing_tooltips))
                    else:
                        print("Tooltips for the following regions not found: " + ", ".join(missing_tooltips))                
                        print("Polygons for the following regions not found: " + ", ".join(missing_polygons))
                
            # Special process for Bars
            if self.type == "Bars":

                # Defining unique colors
                country_borders = dict(zip(unique_colors["nuts_id"], unique_colors["unique_border"]))
                country_labels  = dict(zip(unique_colors["nuts_id"], unique_colors["unique_label"]))

                # Creating IDs for bars
                for nuts, color_code in country_borders.items():
                    for element in root.xpath(".//*[@style]", namespaces = namespaces):
                        if color_code in element.attrib["style"]:
                            element.set("id", f"{nuts}_bar")

                # Bagging tooltips elements into a single regional <g> tag
                for nuts, color_code in country_labels.items():
                    nuts_group = ET.Element("g", id = f"{nuts}_tooltip")
                    first_found = False  

                    for element in root.xpath(".//*[@style]", namespaces = namespaces):

                        if color_code in element.attrib["style"]:

                            original_parent = element.getparent()
                            original_parent.remove(element)

                            if not first_found:
                                original_parent.append(nuts_group)
                                first_found = True

                            nuts_group_element = original_parent.find(f".//g[@id='{nuts}_tooltip']")
                            nuts_group_element.append(element)
                
                # Hide tooltips by default
                missing_tooltips = []
                for nuts in unique_colors["nuts_id"].to_list():
                    tooltip = root.find(f".//g[@id='{nuts}_tooltip']")
                    if tooltip is not None:
                        tooltip.set("visibility", "hidden")
                    else:
                        missing_tooltips.append(nuts)
                
                # Assign onmouseover and onmouseout callbacks to patches.
                missing_bars = []
                for nuts in unique_colors["nuts_id"].to_list():
                    bar = root.xpath(f"//svg:rect[@id='{nuts}_bar']", namespaces=namespaces)
                    if bar:
                        bar[0].set("onmouseover", "ShowTooltip(this)")
                        bar[0].set("onmouseout", "HideTooltip(this)")
                    else:
                        missing_bars.append(nuts)
                
                if missing_tooltips or missing_bars:
                    if len(missing_tooltips) == len(missing_bars):
                        print("Data for the following countries not found: " + ", ".join(missing_tooltips))
                    else:
                        print("Tooltips for the following countries not found: " + ", ".join(missing_tooltips))                
                        print("Circles for the following countries not found: " + ", ".join(missing_bars))

            # Special process for Dots
            if self.type in ["Dots"]:

                # Defining unique colors
                region_borders = dict(zip(unique_colors["nuts_id"], unique_colors["unique_border"]))
                region_labels  = dict(zip(unique_colors["nuts_id"], unique_colors["unique_label"]))

                # Bagging points elements into a single regional <g> tag
                for nuts, color_code in region_borders.items():

                    nuts_group = ET.Element("g", id = f"{nuts}_points")
                    first_found = False  

                    for element in root.xpath(".//*[@style]", namespaces = namespaces):

                        if color_code in element.attrib["style"]:
                            
                            original_parent = element.getparent()
                            original_parent.remove(element)

                            if not first_found:
                                original_parent.append(nuts_group)

                            nuts_group_element = original_parent.find(f".//g[@id='{nuts}_points']")
                            nuts_group_element.append(element)
            
                # Bagging tooltip elements into a single regional <g> tag
                for nuts, color_code in region_labels.items():

                    nuts_group = ET.Element("g", id = f"{nuts}_tooltip")
                    first_found = False

                    for element in root.xpath(".//*[@style]", namespaces = namespaces):
                        
                        if color_code in element.attrib["style"]:
                            original_parent = element.getparent()
                            if not first_found:
                                original_parent.append(nuts_group)
                                first_found = True
                            nuts_group_element = root.find(f".//g[@id='{nuts}_tooltip']")

                            element_siblings = list(element.itersiblings(preceding = False))
                            for sibling in element_siblings:
                                if "polygon" in sibling.tag:
                                    break
                                if "path" in sibling.tag:
                                    original_parent.remove(sibling)
                                    nuts_group_element.append(sibling)

                            original_parent.remove(element)
                            nuts_group_element.append(element)
                 
                # Hide tooltips by default
                missing_tooltips = []
                for nuts in unique_colors["nuts_id"].to_list():
                    tooltip = root.find(f".//g[@id='{nuts}_tooltip']")
                    if tooltip is not None:
                        tooltip.set('visibility', 'hidden')
                    else:
                        missing_tooltips.append(nuts)

                # Assign onmouseover and onmouseout callbacks to patches.
                missing_points = []
                for nuts in unique_colors["nuts_id"].to_list():
                    points = root.find(f".//g[@id='{nuts}_points']")
                    if points is not None:
                        points.set('onmouseover', "ShowTooltip(this)")
                        points.set('onmouseout', "HideTooltip(this)")
                    else:
                        missing_points.append(nuts)

                if missing_tooltips or missing_points:
                    if len(missing_tooltips) == len(missing_points):
                        print("Data for the following regions not found: " + ", ".join(missing_tooltips))
                    else:
                        print("Tooltips for the following regions not found: " + ", ".join(missing_tooltips))                
                        print("Polygons for the following regions not found: " + ", ".join(missing_points))
                
            # We need a JS that can modify the visibility attributes of the tooltips
            script = """
                <script type="text/ecmascript">
                <![CDATA[

                function init(event) {
                    if ( window.svgDocument == null ) {
                        svgDocument = event.target.ownerDocument;
                        }
                    }

                function ShowTooltip(obj) {
                    var cur = obj.id.split("_")[0];
                    var tip = svgDocument.getElementById(cur + '_tooltip');
                    tip.setAttribute('visibility', "visible")
                    }

                function HideTooltip(obj) {
                    var cur = obj.id.split("_")[0];
                    var tip = svgDocument.getElementById(cur + '_tooltip');
                    tip.setAttribute('visibility', "hidden")
                    }

                ]]>
                </script>
                """

            # Insert the script at the bottom of the file and save it.
            script_element = ET.fromstring(script)
            root.append(script_element)

            # Write the modified tree to a new file
            tree.write(
                target, 
                xml_declaration = True, 
                encoding        = "utf-8"
            )
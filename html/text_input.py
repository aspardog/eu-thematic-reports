import re
import pandas as pd

class text_input:

    def __init__ (self, report, input_type):
        self.report     = report
        self.input_type = input_type[:-3]
        self.filepath   = f"{report}/text/{input_type}"
        self.text       = self._load_input()

    def _load_input(self):
        with open(self.filepath, "r", encoding="utf-8") as file:
            txt = file.read()
        return txt
    
    def _clean_text(self):
        clean_text = re.sub(r'^# .*\n?', '', self.text, flags=re.MULTILINE).strip()
        return clean_text

    def get_front_page(self):

        if self.report == "R1":
            title = "Democracy & Fundamental Rights"
        if self.report == "R2":
            title = "Justice & Safety"
        if self.report == "R3":
            title = "Transparency & Corruption"

        elements = ["front_page_theme", "front_page_title", "front_page_image", "front_page_year", "front_page_source"]
        ids      = [1,2,3,4,5]
        content  = ["Thematic Report", title, "cover.jpg", "2024", "World Justice Project EUROVOICES"]

        front_page = pd.DataFrame.from_dict(
            {
                "id"      : ids,
                "type"    : elements,
                "content" : content
            }
        )

        title    = re.search(r"^# (.*)", self.text, re.MULTILINE).group(1)
        scrolls  = re.findall(r"^(## .*)", self.text, re.MULTILINE)
        keyfinds = re.findall(r"^(#### .*)", self.text, re.MULTILINE)

        id_1 = [6,7,8,9]
        type_1 = ["title"] + ["scrollytelling"] * len(scrolls)
        content_1 = [title] + scrolls 
        df1 = pd.DataFrame.from_dict(
            {
                "id"      : id_1,
                "type"    : type_1,
                "content" : content_1
            }
        )

        id_2 = [7,8,9]
        type_2 = ["scrollytelling_img"] * len(scrolls)
        content_2 = [f"scroll{i+1}.png" for i in range(len(scrolls))]
        df2 = pd.DataFrame.from_dict(
            {
                "type"       : type_2,
                "content"    : content_2,
                "belongs_to" : id_2
            }
        )

        id_3 = [10+i for i in range(len(keyfinds))]
        type_3 = "keyfinding"
        content_3 = keyfinds
        df3 = pd.DataFrame.from_dict(
            {
                "id"         : id_3,
                "type"       : type_3,
                "content"    : content_3,
            }
        )

        output = pd.concat([front_page, df1, df2, df3])

        return output
    
    def get_intro_sections(self, start_id):

        fixed_ids = {
            "introduction"      : start_id,
            "executive-summary" : start_id+1
        }

        clean_text = self._clean_text()
        output     = pd.DataFrame(
            {
                "id"      : fixed_ids[self.input_type],
                "type"    : self.input_type,
                "content" : clean_text
            },
            index = [0]
        ) 

        return output
    
    def get_thematic_findings(self, chartsdata, pdfver, id_start):

        if self.report == "R1":
            title = "Democracy & Fundamental Rights"
        if self.report == "R2":
            title = "Justice & Safety"
        if self.report == "R3":
            title = "Transparency & Corruption"

        id_counter = id_start
        
        # Creating an empty list to store the results
        data = []
        
        # Cleaning text and splitting chapters
        clean_text = self._clean_text()
        chapters = (
            re.compile(r'(## .+?)(?=\n## |\Z)', re.DOTALL)
            .findall(clean_text)
        )

        # Looping over chapters
        for chapter in chapters:

            chapter_id = id_counter

            chapter_content = chapter.split("\n", 1)
            main_header     = chapter_content[0].strip("##").strip()
            header_text     = re.sub(r"^\d+\. ", "", main_header)
            chapter_data = {
                "id"         : chapter_id,
                "type"       : "chapter",
                "content"    : f"# {header_text}",
                "belongs_to" : None,
                "settings"   : None
            }
            id_counter += 1
            
            sub_content = chapter_content[1].strip() if len(chapter_content) > 1 else ""
            findings_1  = re.search(r'^(.*?)(?=###)', sub_content, re.DOTALL).group().strip()
            findings_data_1 = {
                "id"         : id_counter,
                "type"       : "findings",
                "content"    : findings_1,
                "belongs_to" : chapter_id,
                "settings"   : None
            }
            findings_data_1_pdfv = {
                "id"         : id_counter,
                "type"       : "findings",
                "content"    : chapter_content[1].strip(),
                "belongs_to" : chapter_id,
                "settings"   : None
            }
            id_counter += 1

            if not pdfver:
                data.extend([chapter_data, findings_data_1])
            else:
                data.extend([chapter_data, findings_data_1_pdfv])

            sections = (
                re.compile(r"(### .+?)(?=\n### |\n## |\Z)", re.DOTALL)
                .findall(sub_content)
            )

            # Looping over sections
            for sub_section in sections:
                section_id = id_counter
                sub_lines  = sub_section.split('\n', 1)
                sub_header = sub_lines[0].strip('### ').strip()
                sub_text   = sub_lines[1].strip() if len(sub_lines) > 1 else ""
                section_data = {
                    "id"         : section_id,
                    "type"       : "sub_chapter",
                    "content"    : f"## {sub_header}",
                    "belongs_to" : chapter_id,
                    "settings"   : None
                }
                id_counter += 1

                findings_data_2 = {
                    "id"         : id_counter,
                    "type"       : "html",
                    "content"    : sub_text,
                    "belongs_to" : section_id,
                    "settings"   : None
                }
                findings_data_2_pdfv = {
                    "id"         : id_counter,
                    "type"       : "html",
                    "content"    : "\n",
                    "belongs_to" : section_id,
                    "settings"   : None
                }
                id_counter += 1
                
                if not pdfver:
                    data.extend([section_data, findings_data_2])
                else:
                    data.extend([section_data, findings_data_2_pdfv])

                charts4section = (
                    chartsdata.copy()
                    .loc[(chartsdata["report"] == title) & (chartsdata["section"] == sub_header)]
                )

                counter4config = 1

                # Looping over charts
                for _, row in charts4section.iterrows():
                    if row["description"] == "QRQ":
                        category = "Expert's Scorecard"
                        setting  = "expert"
                    if row["description"] == "GPP":
                        category = "People's Voices"
                        setting  = "people"
                    # chart_n = re.search("(?<=F).+", row["chart_id"]).group()
                    # if len(chart_n) < 2:
                    #     figid = re.sub("(?<=F).+", f"0{chart_n}", row["chart_id"])
                    # else:
                    #     figid = row["chart_id"]
                    figid = id_counter
                    id_counter += 1
                    counter4config += 1

                    accordion_viz = {
                        "id"         : figid,
                        "type"       : "accordion_viz",
                        "content"    : f"<span>{row['figure']}</span> {row['title']}",
                        "belongs_to" : section_id,
                        "settings"   : setting
                    }
                    accordion_viz_category = {
                        "id"         : None,
                        "type"       : "accordion_viz_category",
                        "content"    : category,
                        "belongs_to" : figid,
                        "settings"   : None
                    }
                    accordion_viz_description = {
                        "id"         : id_counter,
                        "type"       : "accordion_viz_description",
                        "content"    : row["subtitle"],
                        "belongs_to" : figid,
                        "settings"   : None
                    }
                    id_counter += 1
                    accordion_viz_image = {
                        "id"         : id_counter,
                        "type"       : "accordion_viz_image",
                        "content"    : f"{row['chart_id']}.svg",
                        "belongs_to" : figid,
                        "settings"   : None,
                        "id4config"  : re.sub(r"\.0", "", f"image_{chapter_id}_{section_id}_{counter4config}")
                    }
                    id_counter += 1
                    accordion_viz_note = {
                        "id"         : id_counter,
                        "type"       : "accordion_viz_note",
                        "content"    : row["footnote"],
                        "belongs_to" : figid,
                        "settings"   : None
                    }
                    id_counter += 1

                    data.extend([accordion_viz, accordion_viz_category, accordion_viz_description, accordion_viz_image, accordion_viz_note])
                
            output = pd.DataFrame.from_dict(data)
        
        return output

    def get_final_sections(self, start_id):

        if self.input_type == "methodology":
            input = "methodological"
        else:
            input = self.input_type

        fixed_ids = {
            "appendix"         : start_id,
            "methodology"      : start_id+1,
            "about"            : start_id+2,
            "acknowledgements" : start_id+3
        }

        clean_text = self._clean_text()
        output     = pd.DataFrame(
            {
                "id"      : fixed_ids[self.input_type],
                "type"    : input,
                "content" : clean_text
            },
            index = [0]
        ) 
        
        return output
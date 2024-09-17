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
            title = "Corruption & Transparency"

        elements = ["front_page_theme", "front_page_title", "front_page_image", "front_page_year", "front_page_source"]
        content  = ["Thematic Report", title, "cover.jpg", "2024", "World Justice Project EUROVOICES"]

        output = pd.DataFrame.from_dict(
            {
                "id"      : elements,
                "type"    : elements,
                "content" : content
            }
        )

        return output

    def get_csv(self, chartsdata):

        if self.input_type == "title-&-scroll":
            title   = re.search(r"^# (.*)", self.text, re.MULTILINE).group(1)
            scrolls = re.findall(r"^(## .*)", self.text, re.MULTILINE)

            id_1 = ["title"] + [f"scroll{i+1}" for i in range(len(scrolls))]
            type_1 = ["title"] + ["scrollytelling"] * len(scrolls)
            content_1 = [title] + scrolls 
            df1 = pd.DataFrame.from_dict(
                {
                    "id"      : id_1,
                    "type"    : type_1,
                    "content" : content_1
                }
            )

            id_2 = [f"scroll{i+1}" for i in range(len(scrolls))]
            type_2 = ["scrollytelling_img"] * len(scrolls)
            content_2 = [f"scroll{i+1}.png" for i in range(len(scrolls))]
            df2 = pd.DataFrame.from_dict(
                {
                    "type"       : type_2,
                    "content"    : content_2,
                    "belongs_to" : id_2
                }
            )

            output = pd.concat([df1, df2])
        
        if self.input_type in ["introduction", "executive-summary", "about", "methodology", "acknowledgements", "appendix"]:

            if self.input_type == "methodology":
                input = "methodological"
            else:
                input = self.input_type

            clean_text = self._clean_text()
            output     = pd.DataFrame(
                {
                    "id"      : self.input_type,
                    "type"    : input,
                    "content" : clean_text
                },
                index = [0]
            ) 
        
        if self.input_type == "thematic-findings":

            if self.report == "R1":
                title = "Democracy & Fundamental Rights"
            if self.report == "R2":
                title = "Justice & Safety"
            if self.report == "R3":
                title = "Corruption & Transparency"
            
            # Creating an empty list to store the results
            data = []

            # Cleaning text and splitting chapters
            clean_text = self._clean_text()
            chapters = (
                re.compile(r'(## .+?)(?=\n## |\Z)', re.DOTALL)
                .findall(clean_text)
            )

            # Looping over chapters
            chapter_no = 1
            for chapter in chapters:

                chapter_content = chapter.split("\n", 1)
                main_header = chapter_content[0].strip("##").strip()
                if chapter_no < 10:
                    chapter_n = f"0{chapter_no}"
                else:
                    chapter_n = f"{chapter_no}"
                chapter_no += 1
                header_text = re.sub(r"^\d+\. ", "", main_header)
                chapter_data = {
                    "id"         : f"ch{chapter_n}",
                    "type"       : "chapter",
                    "content"    : f"# {header_text}",
                    "belongs_to" : None,
                    "settings"   : None
                }
                
                sub_content = chapter_content[1].strip() if len(chapter_content) > 1 else ""
                findings_1  = re.search(r'^(.*?)(?=###)', sub_content, re.DOTALL).group().strip()
                findings_data_1 = {
                    "id"         : f"ch{chapter_n}_f",
                    "type"       : "findings",
                    "content"    : findings_1,
                    "belongs_to" : f"ch{chapter_n}",
                    "settings"   : None
                }

                data.extend([chapter_data, findings_data_1])

                sections = (
                    re.compile(r"(### .+?)(?=\n### |\n## |\Z)", re.DOTALL)
                    .findall(sub_content)
                )

                section_no = 1
                # Looping over sections
                for sub_section in sections:
                    sub_lines  = sub_section.split('\n', 1)
                    sub_header = sub_lines[0].strip('### ').strip()
                    sub_text   = sub_lines[1].strip() if len(sub_lines) > 1 else ""
                    # section_n  = re.match(r"^\d+", sub_header).group().strip() 
                    # section_n  =  re.sub("\.", "", re.search(r'<span[^>]*>(.*?)</span>', sub_header).group(1).strip())
                    section_n = f"{section_no}"
                    if section_no < 10:
                        section_n = f"0{section_n}"
                    section_no += 1
                    section_data = {
                        "id"         : f"ch{chapter_n}_sec{section_n}",
                        "type"       : "sub_chapter",
                        "content"    : f"## {sub_header}",
                        "belongs_to" : f"ch{chapter_n}",
                        "settings"   : None
                    }
                    findings_data_2 = {
                        "id"         : f"f_ch{chapter_n}_sec{section_n}",
                        "type"       : "html",
                        "content"    : sub_text,
                        "belongs_to" : f"ch{chapter_n}_sec{section_n}",
                        "settings"   : None
                    }

                    data.extend([section_data, findings_data_2])

                    # match_in_outline = re.match(r"\d+\.\s*(.*)", sub_header).group(1).strip()
                    # match_in_outline = re.split(r'</span>', sub_header)[1].strip()
                    charts4section = (
                        chartsdata.copy()
                        .loc[(chartsdata["report"] == title) & (chartsdata["section"] == sub_header)]
                    )

                    # Looping over charts
                    for index, row in charts4section.iterrows():
                        if row["description"] == "QRQ":
                            category = "Expert's Scorecard"
                            setting  = "expert"
                        if row["description"] == "GPP":
                            category = "People's Voices"
                            setting  = "people"
                        chart_n = re.search("(?<=F).+", row["chart_id"]).group()
                        if len(chart_n) < 2:
                            figid = re.sub("(?<=F).+", f"0{chart_n}", row["chart_id"])
                        else:
                            figid = row["chart_id"]
                        figid = f"sec{section_n}_{figid}"

                        accordion_viz = {
                            "id"         : figid,
                            "type"       : "accordion_viz",
                            "content"    : f"<span>{row['figure']}</span> {row['title']}",
                            "belongs_to" : f"ch{chapter_n}_sec{section_n}",
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
                            "id"         : f"{figid}_d",
                            "type"       : "accordion_viz_description",
                            "content"    : row["subtitle"],
                            "belongs_to" : figid,
                            "settings"   : None
                        }
                        accordion_viz_image = {
                            "id"         : f"{figid}_i",
                            "type"       : "accordion_viz_image",
                            "content"    : f"{row['chart_id']}.svg",
                            "belongs_to" : figid,
                            "settings"   : None
                        }
                        accordion_viz_note = {
                            "id"         : f"{figid}_n",
                            "type"       : "accordion_viz_note",
                            "content"    : None,
                            "belongs_to" : figid,
                            "settings"   : None
                        }

                        data.extend([accordion_viz, accordion_viz_category, accordion_viz_description, accordion_viz_image, accordion_viz_note])
                
            output = pd.DataFrame.from_dict(data)
        
        return output
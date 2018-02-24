let update_html_content elt content =
  let dom_html = Eliom_content.Html5.To_dom.of_element elt in
  ignore (React.E.map
            (fun c -> dom_html##.innerHTML := Js.string c)
            content)

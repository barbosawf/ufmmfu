# Built-in vegetation index equation table ---------------------------------
#
# This table used to live in an external CSV/XLSX file that every analysis
# script had to load by hand (e.g. readr::read_delim("Data/CSV/VIs.csv")).
# Bundling it as get_vi_table() means it ships with the package itself --
# no external file to keep in sync or re-upload -- and is ready to pass
# straight into vis_df arguments across the package (calc_vis_4(),
# calc_vis_rast(), get_raster_data(), get_gee_data()).
#
# The raw-band "pseudo-indices" (R, G, B, RE, NIR, SWIR -- rows whose
# Equation was just their own symbol) that existed in the original
# spreadsheet as placeholders, not real indices, are already left out below
# -- every script that has used this table has always filtered them out
# before computing indices, so the table here is the already-usable result,
# not the raw spreadsheet dump.


#' Return the package's built-in table of vegetation index equations
#'
#' Returns a \code{tibble} with columns \code{Index}, \code{Equation} and
#' \code{Band} -- one row per vegetation index, ready to use as-is as the
#' \code{vis_df} argument of \code{\link{calc_vis_4}},
#' \code{\link{calc_vis_rast}}, \code{\link{get_raster_data}} or
#' \code{\link{get_gee_data}}. \code{Band} groups indices by the sensor/
#' color-space family they were written for: \code{"MULTI"} (multispectral
#' formulas referencing \code{R}/\code{G}/\code{B}/\code{RE}/\code{NIR}/
#' \code{SWIR}), \code{"RGB"} (visible-only formulas), or \code{"CIELab"}
#' (formulas built on the CIELAB \code{L*}/\code{a*}/\code{b*} color
#' space, themselves derived from \code{R}/\code{G}/\code{B}).
#'
#' @return A \code{tibble} with columns \code{Index}, \code{Equation},
#'   \code{Band}.
#'
#' @examples
#' get_vi_table()
#'
#' @export
get_vi_table <- function() {

  tibble::tribble(
    ~Index, ~Equation, ~Band,
    "L*", "(0.2126*R+0.7152*G+0.0722*B)", "CIELab",
    "L*-a", "((0.2126*R+0.7152*G+0.0722*B))-(0.55*((R-(0.2126*R+0.7152*G+0.0722*B))/(1.0-0.2126)))", "CIELab",
    "L*-b", "((0.2126*R+0.7152*G+0.0722*B))-(0.55*((B-(0.2126*R+0.7152*G+0.0722*B))/(1.0-0.0722)))", "CIELab",
    "a", "0.55*((R-(0.2126*R+0.7152*G+0.0722*B))/(1.0-0.2126))", "CIELab",
    "b*", "0.55*((B-(0.2126*R+0.7152*G+0.0722*B))/(1.0-0.0722))", "CIELab",
    "b*-a", "(0.55*((B-(0.2126*R+0.7152*G+0.0722*B))/(1.0-0.0722)))-(0.55*((R-(0.2126*R+0.7152*G+0.0722*B))/(1.0-0.2126)))", "CIELab",
    "ARI", "(1/G)-(1/RE)", "MULTI",
    "ARVI", "(NIR-(R-0.1*(R-B)))/(NIR+(R-0.1*(R-B)))", "MULTI",
    "BAI", "1/((0.1-R)^2+(0.06-NIR)^2)", "MULTI",
    "BWDRVI", "(0.1*NIR-B)/(0.1*NIR+B)", "MULTI",
    "CCCI", "((NIR-R)/(NIR+R))/((NIR-R)/(NIR+R))", "MULTI",
    "CIG", "(NIR/G)-1", "MULTI",
    "CIRE", "(NIR/RE)-1", "MULTI",
    "CVI", "NIR*(R/(G*G))", "MULTI",
    "EVI", "2.5*(NIR-R)/(NIR+6*R-7.5*B+1)", "MULTI",
    "GARI", "(NIR-(1.7*(B-R)))/(NIR+(1.7*(B-R)))", "MULTI",
    "GDVI", "NIR-G", "MULTI",
    "GEMI", "(2*(NIR*NIR-R*R)+1.5*NIR+0.5*R)/(NIR+R+0.5)*(1-0.25*(2*(NIR*NIR-R*R)+1.5*NIR+0.5*R)/(NIR+R+0.5))-((R-0.125)/(1-R))", "MULTI",
    "GNDVI", "(NIR-G)/(NIR+G)", "MULTI",
    "GOSAVI", "(NIR-G)/(NIR+G+0.16)", "MULTI",
    "GRVI", "NIR/G", "MULTI",
    "GSAVI", "((NIR-G)/(NIR+G+0.5))*(1+0.5)", "MULTI",
    "IPVI", "NIR/(NIR+R)", "MULTI",
    "LAI", "3.368*(2.5*(NIR-R)/(NIR+6*R-7.5*B+1))-0.118", "MULTI",
    "MCARI1", "1.2*((2.5*(NIR-R))-(1.3*(NIR-G)))", "MULTI",
    "MCARI2", "(1.2*(2.5*(NIR-R)-1.3*(NIR-G))/sqrt((2*NIR+1)^2-(6*NIR-5*sqrt(R))-0.5))", "MULTI",
    "MNDWI", "(G-SWIR)/(G+SWIR)", "MULTI",
    "MSAVI", "(1/2)*(2*(NIR+1)-sqrt((2*NIR+1)*2-8*(NIR-R)))", "MULTI",
    "MSAVI2", "(2*NIR+1-sqrt((2*NIR+1)^2-8*(NIR-R)))/2", "MULTI",
    "MSR", "(NIR/R-1)/(sqrt(NIR/R)+1)", "MULTI",
    "NBR", "(NIR-SWIR)/(NIR+SWIR)", "MULTI",
    "NDMI", "(NIR-SWIR)/(NIR+SWIR)", "MULTI",
    "NDRE", "(NIR-RE)/(NIR+RE)", "MULTI",
    "NDVI", "(NIR-R)/(NIR+R)", "MULTI",
    "NDWI", "(G-NIR)/(G+NIR)", "MULTI",
    "NLI", "(NIR^2-R)/(NIR^2+R)", "MULTI",
    "OSAVI", "(NIR-R)/(NIR+R+0.16)", "MULTI",
    "PNDVI", "((NIR-(G+R+B))/(NIR+(G+R+B)))", "MULTI",
    "PSRI", "(R-G)/RE", "MULTI",
    "RDVI", "(NIR-R)/(sqrt(NIR+R))", "MULTI",
    "REDVI", "NIR-RE", "MULTI",
    "RESR", "NIR/RE", "MULTI",
    "RVI", "R/NIR", "MULTI",
    "SAVI2", "((NIR-R)/(NIR+R+0.5)*1+0.5)", "MULTI",
    "TCARI", "3*((RE-R)-0.2*(RE-G)*(RE/R))", "MULTI",
    "TDVI", "1.5*((NIR-R)/(sqrt(NIR^2+R+0.5)))", "MULTI",
    "TSAVI", "(2*((NIR-2)*(R-1)))/(R+2*(NIR-1)+0.5*(1+2*2))", "MULTI",
    "TVI", "sqrt((NIR-R)/(NIR+R)+0.5)", "MULTI",
    "VARIRE", "(RE-1.7*R+0.7*B)/(RE+2.3*R-1.3*B)", "MULTI",
    "VIG", "(NIR-G)/(NIR+G)", "MULTI",
    "VIN", "NIR/R", "MULTI",
    "VIRE", "(RE-R)/(RE+R)", "MULTI",
    "WDRVI", "(0.2*NIR-R)/(0.2*NIR+R)", "MULTI",
    "WSI", "5*(NIR-R)/(NIR+R)-(RE-NIR)/(RE+NIR)", "MULTI",
    "B-G", "B-G", "RGB",
    "B-R", "B-R", "RGB",
    "BGI", "B/G", "RGB",
    "BI", "sqrt((R^2+G^2+B^2)/3)", "RGB",
    "BIM", "sqrt((R*2+G*2+B*2)/3)", "RGB",
    "CI", "((R-B)/R)", "RGB",
    "CIVE", "(0.811*G)+(0.385*B)+18.78745", "RGB",
    "EGVI", "2*G-R-B", "RGB",
    "ERVI", "((1.4*R)-G)", "RGB",
    "ExB", "1.4*B-G", "RGB",
    "ExGR", "(2*G-R-B)/(1.4*R-G)", "RGB",
    "ExR", "1.4*R-G", "RGB",
    "G-B", "G-B", "RGB",
    "G-R", "G-R", "RGB",
    "GB", "G/B", "RGB",
    "GD", "G-(R+B)/2", "RGB",
    "GLAI", "(25*(G-R)/(G+R-B)+1.25)", "RGB",
    "GLI", "((G-R)+(G-B))/(G+R+G+B)", "RGB",
    "GLI2", "(2*G-R+B)/(2*G+R+B)", "RGB",
    "GR", "G/R", "RGB",
    "GRAY", "0.299*R+0.587*G+0.114*B", "RGB",
    "GRAY2", "((R^2.2+(1.5*G)^2.2+(0.6*B)^2.2)/(1+1.5^2.2+0.6^2.2))^(1/2.2)", "RGB",
    "HI", "(2*R-G-B)/(G-B)", "RGB",
    "HUE", "atan(2*(B-G-R)/30.5*(G-R))", "RGB",
    "HUE2", "atan(2*(R-G-R)/30.5*(G-B))", "RGB",
    "I", "R+G+B", "RGB",
    "L", "(R+G+B)/3", "RGB",
    "MGVRI", "(G^2-R^2)/(G^2+R^2)", "RGB",
    "MyIndex", "2*(R-G)/(1+(R+G))", "RGB",
    "NB", "B/(R+G+B)", "RGB",
    "NG", "G/(R+G+B)", "RGB",
    "NGBDI", "(G-B)/(G+B)", "RGB",
    "NGRDI", "(G-R)/(G+R)", "RGB",
    "NR", "R/(R+G+B)", "RGB",
    "R-B", "R-B", "RGB",
    "R-G", "R-G", "RGB",
    "RB", "R/B", "RGB",
    "RGBVI", "(G^2-B*R^2)/(G^2+B*R^2)", "RGB",
    "RGRI", "R/G", "RGB",
    "RGVBI", "(G-(B*R))/(G^2+(B*R))", "RGB",
    "RI", "(R^2/(B*G^3))", "RGB",
    "S", "((R+G+B)-3*B)/(R+G+B)", "RGB",
    "SAVI", "(1+0.5)*(G-R)/(G+R+0.5)", "RGB",
    "SCI", "(R-G)/(R+G)", "RGB",
    "SHP", "(2*(R-G-B)/(G-B))", "RGB",
    "SI", "(R-B)/(R+B)", "RGB",
    "VARI", "(G-R)/(G+R-B)", "RGB",
    "VEG", "G/((R^0.667)*(B^(1-0.667)))", "RGB"
  )

}

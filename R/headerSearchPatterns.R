headerSearchPatterns<-function()
{
    
    patterns=c("N=(?<capture>[0-9]*) spheres",
    "nominal packing density = (?<capture>[0-9]*\\.?[0-9]*)",
    "Young modulus of the individual spheres = (?<capture>[0-9]*\\.?[0-9]*) Pa",
    "Friction coefficient = (?<capture>[0-9]*\\.?[0-9]*)",
    "do_permanent_links=(?<capture>[a-zA-Z]*),",
    "cut_lines=(?<capture>[0-9]*),",
    "amplitude=(?<capture>[0-9]*\\.?[0-9]*),",
    "baseline_pre_periods = (?<capture>[0-9]*)",
    "baseline_post_periods = (?<capture>[0-9]*)",
    "runSimulation\\(periods=(?<capture>[0-9]*),",
    "relative_transversal_link_strength=(?<capture>[0-9]*\\.?[0-9]*)[,\\)]",
    "relative_viscosity=(?<capture>[0-9]*\\.?[0-9]*)",
    "Damping factor = (?<capture>[0-9]*\\.?[0-9]*)",
    "G'=(?<capture>-?[0-9]*\\.?[0-9]*)",
    "G''=(?<capture>-?[0-9]*\\.?[0-9]*)",
    "G'\\(by surface force\\)=(?<capture>-?[0-9]*\\.?[0-9]*)",
    "G''\\(by surface force\\)=(?<capture>-?[0-9]*\\.?[0-9]*)",
    "relative_frequency=(?<capture>[0-9]*\\.?[0-9]*)",
    "Frequency = (?<capture>[0-9]*\\.?[0-9]*)Hz",
    "avoid_horizontal_angle_degree=(?<capture>[0-9]*\\.?[0-9]*)",
    "Stress = (?<capture>[0-9]*\\.?[0-9e\\-]*)Pa",
    "interface_reenforcement_central=(?<capture>[0-9]*\\.?[0-9]*)",
    "interface_reenforcement_tangential=(?<capture>[0-9]*\\.?[0-9]*)",
    "keep_viscosity_coefficients_constant=(?<capture>[a-zA-Z]*)",
    "cut_top_bottom=(?<capture>[a-zA-Z]*)",
    "doCutByTriangulation=(?<capture>[a-zA-Z]*)",
    "remove_link_fraction=(?<capture>[0-9]*\\.?[0-9]*)",
    "Actual fraction of randomly removed non-essential permanent bonds = (?<capture>[0-9]*\\.?[0-9]*)")
    
    names(patterns)=c("N","packing_fraction",
    "Young_modulus_spheres","Friction_coefficient","do_permanent_links",
    "cut_lines","Strain_amplitude","baseline_pre_periods","baseline_post_periods",
    "periods","relative_transversal_link_strength","relative_viscosity","damping_factor_per_dt",
    "Gprime_Pa","Gprimeprime_Pa",
    "Gprime_Pa_surface_force","Gprimeprime_Pa_surface_force",
    "relative_frequency","Frequency","avoid_horizontal_angle_degree","Stress",
    "interface_reenforcement_central","interface_reenforcement_tangential",
    "keep_viscosity_coefficients_constant",
    "cut_top_bottom","doCutByTriangulation","remove_link_fraction","actual_remove_link_fraction")
    
    return(patterns)
    
}
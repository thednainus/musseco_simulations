using Coalescent 
using Plots
using Phylo 
using Distributions
using StatsBase 
using Interpolations
using DataFrames
using CSV 

sampsize = 4056;
ntr = 2 ; # number of combination of parameter values

#read parameter values
params = CSV.read("hiv_params.csv", DataFrame; delim = ',')


m = ModelFGY( "musseco2.yaml" )




function gensampconf(m::ModelFGY)

	o = solveodes(m)

	plot(o)

	staxis = range(996, maximum(o.t), length=20)  # o.t[ o.t .> 16 ] 
	ouinterp1 = linear_interpolation( o.t , map(x->x[1],o.u) )
	ouinterp2 = linear_interpolation( o.t , map(x->x[2],o.u) )
	ouaxis = last( o.u, length(staxis))

	Nwt =  sum( ouinterp1.(staxis) )
	Ntfp = sum( ouinterp2.(staxis) )

	nwt = sampsize*Nwt/(Ntfp+ Nwt ) |> floor |> Integer
	ntfp = sampsize*Ntfp/(Ntfp + Nwt ) |> floor |> Integer

	wtsamps  = sample( staxis, ouinterp1.(staxis)|>Weights , nwt; replace=true )
	tfpsamps = sample( staxis, ouinterp2.(staxis)|>Weights , ntfp; replace=true )

	s = SampleConfiguration( [ [ ("A", tt) for tt in wtsamps ]...
	,[ ("V", tt) for tt in tfpsamps ]...
	] )
	s

end

#plot trajectories
map(2:2) do i
	s = params.s[i]
	mu_AV = params.mu_AV[i]
	mu_VA = params.mu_VA[i]
	beta = params.beta[i] * 2

	m.parameters["s"] = s 
	m.parameters["μ_AV"] = mu_AV
	m.parameters["μ_VA"] = mu_VA
	m.parameters["β"] = beta


	@show m.parameters 

	o = solveodes(m)
	
	
	traj_plot = plot(o)
	savefig(traj_plot, "$i.png")
end

# tonewick(tr) |> parsenewick |> plot 

#simulate tree with combination of parameter value 
map(1:ntr) do i
	map(1:50) do j
		s = params.s[i]
		mu_AV = params.mu_AV[i]
		mu_VA = params.mu_VA[i]
		beta = params.beta[i] * 2

		m.parameters["s"] = s 
		m.parameters["μ_AV"] = mu_AV
		m.parameters["μ_VA"] = mu_VA
		m.parameters["β"] = beta

		@show m.parameters 
		sconf = gensampconf( m )
		tr = SimTree( m, sconf ) 
		otr = tonewick(tr)
		mkpath("trees/params_$(i+3)/rep$(j)")
		write( "trees/params_$(i+3)/rep$(j)/param_$(i+3)_rep_$(j).nwk" , otr )
	end
end



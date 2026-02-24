library(hypergeo)

sinucurve = function(z, s, k) {
  #' Sinusoidal Curve
  #' Basis for the Sinusoidal Distribution
  z = as.numeric(z)
  out = numeric(length(z))
  valid = (z > 0 & z < 1)
  if (any(valid)) {
    z_subset = z[valid]
    out[valid] = (sin(pi * z_subset^s))^k
  }
  out
}

.sinuarea_cache = new.env(parent=emptyenv())
sinuarea = function(s, k, cache=T) {
  key = paste(s, k, sep="_")
  if (exists(key, envir=.sinuarea_cache)) {
    return(.sinuarea_cache[[key]])
  }
  if (s==1) {  # see research document
    val = 2^k/pi * beta((k+1)/2, (k+1)/2)
#  } else if (k==round(k) & k>0) {
#    U = 1/s
#    L = 1 + 1/s
#    sum=0
#    for (j in 0:k){
#      z = 1i*pi*(k-2*j)
#      sum = sum + (-1)^j * choose(k,j) * genhypergeo(U, L, z)
#    }
#    return(Re(sum/(2i)^k))
  } else {
    val = integrate(function(z) (sin(pi * z^s))^k,
                   lower=0, upper=1, rel.tol=1e-12)$value
    if (cache) .sinuarea_cache[[key]] = val
  }
  return(val)
}

dsinustd = function(z, s, k, flip=FALSE) {
  #' Sinusoidal PDF
  #' Density of the Sinusoidal Distribution
  norm_const = sinuarea(s, k)
  if (norm_const == 0) return(numeric(length(z)))
  out = numeric(length(z))
  valid = (z > 0 & z < 1)
  if (!any(valid)) return(out)
  z_subset = z[valid]
  if (flip) {
    z.f_subset = 1 - z_subset
  } else {
    z.f_subset = z_subset
  }
  out[valid] = (sin(pi * z.f_subset^s))^k / norm_const
  return(out)
}

.psinustd_cache = new.env(parent=emptyenv())
psinustd = function(z, s, k, flip=FALSE, cache=FALSE) {
  #' Sinusoidal CDF
  vapply(z, FUN.VALUE=numeric(1), FUN=function(z_val) {
    if (flip) {
      return(1 - psinustd(1 - z_val, s=s, k=k, flip=FALSE, cache=cache))
    }
    if (z_val <= 0) return(0)
    if (z_val >= 1) return(1)
    if (cache) {
      key = paste(s, k, z_val, sep="_")
      if (exists(key, envir=.psinustd_cache)) {
        return(.psinustd_cache[[key]])
      }
    }
    integral_result = integrate(dsinustd, 0, z_val, s=s, k=k, flip=FALSE, rel.tol=1e-12)$value
    if (cache) {
      .psinustd_cache[[key]] = integral_result
    }
    return(integral_result)
  })
}

hsinustd = function(z,s,k) {
  return(dsinustd(z, s,k)/(1-psinustd(z,s,k)))
}

dqsinustd.unnorm = function(z,s,k) return(dsinustd(psinustd(z,s,k), s,k))
dqsinustd.area = function(s,k) integrate(dqsinustd.unnorm, s=s, k=k, lower=0, upper=1)$value
dqsinustd = function(z,s,k) dqsinustd.unnorm(z,s,k)/dqsinustd.area(s,k)

.qsinustd_cache = new.env(parent=emptyenv())
qsinustd = function(p, s, k, flip=FALSE, cache=FALSE) {
  #' Sinusoidal Quantile
  vapply(p, FUN.VALUE=numeric(1), FUN=function(p_val) {
    if (flip) {
      return(1 - qsinustd(1 - p_val, s=s, k=k, flip=FALSE, cache=cache))
    }
    
    if (is.na(p_val) || p_val < 0 || p_val > 1) return(NaN)
    if (p_val == 0) return(0)
    if (p_val == 1) return(1)
    
    if (cache) {
      key = paste(s, k, p_val, sep="_")
      if (exists(key, envir=.qsinustd_cache)) {
        return(.qsinustd_cache[[key]])
      }
    }
    objective_f = function(z) {
      psinustd(z, s=s, k=k, flip=FALSE, cache=cache) - p_val
    }
    
    root_result = tryCatch({
      stats::uniroot(objective_f, interval=c(0, 1))$root
    }, error=function(e) { NA })
    
    if (cache) {
      .qsinustd_cache[[key]] = root_result
    }
    return(root_result)
  })
}


rsinustd = function(n, s,k,flip=FALSE) {
  #' Sinusoidal Random Samples
  qsinustd(runif(n), s, k, flip)
}
  
dsinu = function(x, a,d,s,k,flip=F) {
  z = (x-a)/d
  pdf = 1/d * dsinustd(z, s,k,flip)
  return(pdf)
}

psinu = function(x, a,d,s,k,flip=F) {
  z = (x-a)/d
  cdf = psinustd(z, s,k,flip)
  return(cdf)
}

qsinu = function(p, a,d,s,k,flip=F) {
  qf = a + d*qsinustd(p, s,k,flip)
  return(qf)
}

rsinu = function(n, a,d,s,k, flip=F) {
  a + d*rsinustd(n, s, k, flip)
}


sinu.rmom = function(r, a,d,s,k, flip = FALSE) {
  rmom.integrand = function(z) (a+d*z)^r * dsinustd(z, s,k,flip)
  rmomVal = integrate(rmom.integrand, lower=0, upper=1, rel.tol=1e-12)$value
  return(rmomVal)
}

sinu.mean = function(a,d,s,k, flip=FALSE) {
  if (s == 1 || k == 0 || s == 0) {
    return(a + d / 2)
  }
  return(a + d * sinu.rmom(1, 0, 1, s=s, k=k, flip=flip))
}

.sinu.cmom_cache = new.env(parent=emptyenv())
sinu.cmom = function(r, d,s,k, flip = FALSE, cache=FALSE) {
  if (s == 0 || k == 0 || s == 1) {
    if (r %% 2 != 0) return(0)
    flip = FALSE
  }
  key = paste(r, s, k, sep="_")
  if (cache && exists(key, envir=.sinu.cmom_cache)) {
    unscaledCmom = .sinu.cmom_cache[[key]]
  } else {
    stdMean = sinu.mean(0,1,s,k)
    integrand = function(z) (z-stdMean)^r * dsinustd(z, s,k)
    unscaledCmom = integrate(integrand, lower=0, upper=1, rel.tol=1e-12)$value
    if (cache) .sinu.cmom_cache[[key]] = unscaledCmom
  }
  scaledCmom = d^r * unscaledCmom
  if (flip && (r %% 2 != 0)) return(-scaledCmom)
  return(scaledCmom)
}



sinu.var = function(d,s,k) {
  sinu.cmom(2, d, s, k)
}

sinu.skew = function(s,k, flip = FALSE, cache=FALSE) {
  if (s == 0 || k == 0 || s == 1) {
    return(0)
  }
  cmom2 = sinu.cmom(2, 1,s,k, flip, cache=cache)
  cmom3 = sinu.cmom(3, 1,s,k, flip, cache=cache)
  return(cmom3 / cmom2^(3/2))
}

sinu.kurt = function(s,k, cache=FALSE) {
  cmom2 = sinu.cmom(2, 1,s,k, cache=cache)
  cmom4 = sinu.cmom(4, 1,s,k, cache=cache)
  return((cmom4 / cmom2^2) - 3)
}

sinu.msm = function(a,d,s,k, flip = FALSE) {
  measures = c(
    sinu.mean(a, d, s, k, flip),
    sinu.var(d, s, k, flip),
    sinu.skew(s, k, flip),
    sinu.kurt(s, k, flip)
  )
  return(measures)
}

pmfsinu = function(x, a,d,s,k, flip=FALSE) {  # useless function unless we give binning a serious thought
  int.vec = seq.int(floor(a+1), d) # extract integers
  int.vec
  cdf.vec = c(0, psinu(int.vec, a, d, s, k, flip))
  pmfs = diff(cdf.vec)
  pmfs[which(int.vec==x)]
}

myenvir = .psinustd_cache
rm(list=ls(envir=myenvir), envir=myenvir)

save_sinu_caches = function(cache_names, directory = ".") {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  for (cache_name in cache_names) {
    file_name = sub("^\\.", "", cache_name)
    file_path = file.path(directory, paste0(file_name, ".RData"))
    
    if (exists(cache_name, envir = .GlobalEnv)) {
      env_obj = get(cache_name, envir = .GlobalEnv)
      save(list = ls(envir = env_obj), file = file_path, envir = env_obj)
    } else {
      warning(paste("Cache environment not found:", cache_name))
    }
  }
  message(paste("Sinu caches saved to:", directory))
}

load_sinu_caches = function(cache_names, directory = ".") {
  for (cache_name in cache_names) {
    file_name = sub("^\\.", "", cache_name)
    file_path = file.path(directory, paste0(file_name, ".RData"))
    
    if (exists(cache_name, envir = .GlobalEnv)) {
      if (file.exists(file_path)) {
        load(file_path, envir = get(cache_name, envir = .GlobalEnv))
      } else {
        warning(paste("Cache file not found:", file_path))
      }
    } else {
      warning(paste("Cache environment not found:", cache_name))
    }
  }
  message(paste("Sinu caches loaded from:", directory))
}

mycaches = c(".sinuarea_cache", ".psinustd_cache", ".qsinustd_cache", ".sinu.cmom_cache")
#save_sinu_caches(mycaches)
load_sinu_caches(mycaches)

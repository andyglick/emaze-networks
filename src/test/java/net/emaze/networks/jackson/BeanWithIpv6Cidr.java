package net.emaze.networks.jackson;

import net.emaze.networks.Ipv6Network;

public class BeanWithIpv6Cidr {

    private Ipv6Network cidr;

    public Ipv6Network getCidr() {
        return cidr;
    }

    public void setCidr(Ipv6Network cidr) {
        this.cidr = cidr;
    }

}

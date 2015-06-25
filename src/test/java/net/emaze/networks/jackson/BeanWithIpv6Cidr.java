package net.emaze.networks.jackson;

import net.emaze.networks.ipv6.Ipv6Network;

public class BeanWithIpv6Cidr {

    private Ipv6Network cidr;

    public Ipv6Network getCidr() {
        return cidr;
    }

    public void setCidr(Ipv6Network cidr) {
        this.cidr = cidr;
    }

}

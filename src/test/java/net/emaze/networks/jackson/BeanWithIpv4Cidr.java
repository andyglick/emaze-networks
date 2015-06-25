package net.emaze.networks.jackson;

import net.emaze.networks.ipv4.Ipv4Network;

public class BeanWithIpv4Cidr {

    private Ipv4Network cidr;

    public Ipv4Network getCidr() {
        return cidr;
    }

    public void setCidr(Ipv4Network cidr) {
        this.cidr = cidr;
    }

}

package net.emaze.networks.jackson;

import com.fasterxml.jackson.core.Version;
import com.fasterxml.jackson.databind.module.SimpleModule;
import net.emaze.networks.Cidr;
import net.emaze.networks.Ipv4;
import net.emaze.networks.Netmask;

public class NetworksModule extends SimpleModule {

    public NetworksModule() {
        super("networks-module", new Version(1, 0, 0, null, "net.emaze", "networks"));
        this.addSerializer(Ipv4.class, new Ipv4ToString());
        this.addDeserializer(Ipv4.class, new Ipv4FromString());
        this.addSerializer(Netmask.class, new NetmaskToString());
        this.addDeserializer(Netmask.class, new NetmaskFromString());
        this.addSerializer(Cidr.class, new CidrToString());
        this.addDeserializer(Cidr.class, new CidrFromString());
    }
}

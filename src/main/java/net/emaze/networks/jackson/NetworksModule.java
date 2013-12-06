package net.emaze.networks.jackson;

import com.fasterxml.jackson.core.Version;
import com.fasterxml.jackson.databind.module.SimpleModule;
import net.emaze.networks.Network;
import net.emaze.networks.Ip;
import net.emaze.networks.Ipv6;
import net.emaze.networks.Ipv6Mask;
import net.emaze.networks.Ipv6Network;
import net.emaze.networks.Mask;

public class NetworksModule extends SimpleModule {

    public NetworksModule() {
        super("networks-module", new Version(2, 0, 0, null, "net.emaze", "networks"));
        this.addSerializer(Ip.class, new IpToString());
        this.addDeserializer(Ip.class, new IpFromString());
        this.addSerializer(Mask.class, new MaskToString());
        this.addDeserializer(Mask.class, new MaskFromString());
        this.addSerializer(Network.class, new NetworkToString());
        this.addDeserializer(Network.class, new NetworkFromString());
        this.addSerializer(Ipv6.class, new Ipv6ToString());
        this.addDeserializer(Ipv6.class, new Ipv6FromString());
        this.addSerializer(Ipv6Mask.class, new Ipv6MaskToString());
        this.addDeserializer(Ipv6Mask.class, new Ipv6MaskFromString());
        this.addSerializer(Ipv6Network.class, new Ipv6NetworkToString());
        this.addDeserializer(Ipv6Network.class, new Ipv6NetworkFromString());
    }
}

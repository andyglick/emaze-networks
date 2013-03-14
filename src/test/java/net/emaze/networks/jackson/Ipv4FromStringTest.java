package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Ip;
import net.emaze.networks.jackson.Ipv4ToStringTest.BeanWithIpv4;
import org.junit.Test;

public class Ipv4FromStringTest {
    
    @Test
    public void deserializingYieldsExpected() throws IOException {
        final String serialized = "{'ipv4':'127.0.0.1'}".replace("'", "\"");
        final Ip expected = Ip.parse("127.0.0.1");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIpv4 got = mapper.readValue(serialized, BeanWithIpv4.class);
        Assert.assertEquals(expected, got.getIpv4());
    }
}

package net.emaze.networks.jackson;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import java.io.IOException;
import net.emaze.networks.Ipv6Mask;

public class Ipv6MaskToString extends JsonSerializer<Ipv6Mask> {

    @Override
    public void serialize(Ipv6Mask mask, JsonGenerator jg, SerializerProvider sp) throws IOException, JsonProcessingException {
        jg.writeNumber(mask.size());
    }
}
